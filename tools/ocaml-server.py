#!/usr/bin/env python3
import errno
import json
import os
import queue
import socket
import subprocess
import sys
import tempfile
import threading
import time


DEFAULT_TOOL = os.path.basename(sys.argv[0])


def _parse_tool(argv):
    tool = None
    rest = []
    i = 0
    while i < len(argv):
        if argv[i] == "--tool" and i + 1 < len(argv):
            tool = argv[i + 1]
            i += 2
            continue
        rest.append(argv[i])
        i += 1
    return tool or DEFAULT_TOOL, rest


tool_name, argv_rest = _parse_tool(sys.argv[1:])

DEFAULT_STATE_DIR = os.path.join(
    tempfile.gettempdir(), "ocaml_server_shim", tool_name
)
STATE_DIR = os.environ.get("OCAML_SERVER_TMPDIR", DEFAULT_STATE_DIR)
STATE_FILE = os.path.join(STATE_DIR, "state.json")
SOCK_PATH = os.path.join(STATE_DIR, "%s.sock" % tool_name)
LOG_FILE = os.path.join(STATE_DIR, "daemon.log")
START_TIMEOUT = 5.0
DEFAULT_POOL_SIZE = int(os.environ.get("OCAML_SERVER_THREADS", "1"))
DEFAULT_IDLE_TIMEOUT = int(os.environ.get("OCAML_SERVER_IDLE_MINUTES", "1"))
DEBUG = os.environ.get("OCAML_SERVER_DEBUG", "") != ""


server_path = os.path.realpath(sys.argv[0])
server_dir = os.path.dirname(server_path)
REAL_BIN = os.environ.get("OCAML_SERVER_REAL_BIN")
if REAL_BIN is None:
    REAL_BIN = os.environ.get("OCAML_SERVER_BIN")
if REAL_BIN is None:
    candidate_orig = os.path.join(server_dir, tool_name + ".orig")
    candidate_same = os.path.join(server_dir, tool_name)
    if os.path.exists(candidate_orig):
        REAL_BIN = candidate_orig
    elif os.path.exists(candidate_same):
        REAL_BIN = candidate_same
    else:
        REAL_BIN = tool_name


def _log_line(message):
    try:
        os.makedirs(STATE_DIR, exist_ok=True)
        with open(LOG_FILE, "ab", buffering=0) as f:
            line = "[%s] %s\n" % (time.strftime("%Y-%m-%d %H:%M:%S"), message)
            f.write(line.encode("utf-8", errors="replace"))
    except OSError:
        pass


def _connect_socket(path, timeout=0.25):
    s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    s.settimeout(timeout)
    s.connect(path)
    s.settimeout(None)
    return s


def _recv_line(stream):
    line = stream.readline()
    if not line:
        raise EOFError
    return line


def _worker_loop(work_queue):
    try:
        proc = subprocess.Popen(
            [REAL_BIN, "-server"],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=open(LOG_FILE, "ab", buffering=0),
        )
    except OSError as e:
        _log_line("worker failed to start %s: %s" % (REAL_BIN, e))
        return
    try:
        while True:
            item = work_queue.get()
            if item is None:
                break
            line, cwd_line, payload, conn = item
            try:
                proc.stdin.write(line)
                proc.stdin.write(cwd_line)
                proc.stdin.write(payload)
                proc.stdin.flush()

                resp_line = proc.stdout.readline()
                if not resp_line:
                    _log_line("worker saw EOF from %s" % REAL_BIN)
                    conn.close()
                    continue
                resp_parts = resp_line.decode("ascii", errors="replace").strip().split()
                if resp_parts[0] != "RES" or len(resp_parts) != 5:
                    _log_line(
                        "worker got invalid response header: %r" % resp_line
                    )
                    conn.close()
                    continue
                out_len = int(resp_parts[3])
                err_len = int(resp_parts[4])
                data = proc.stdout.read(out_len + err_len)
                if len(data) != out_len + err_len:
                    _log_line(
                        "worker got short response (%d/%d)"
                        % (len(data), out_len + err_len)
                    )
                    conn.close()
                    continue

                conn.sendall(resp_line + data)
            finally:
                conn.close()
    finally:
        proc.terminate()


def _daemon_loop():
    if DEBUG:
        print(
            "ocaml_server_shim: daemon loop starting (pid=%d)" % os.getpid(),
            file=sys.stderr,
        )
    os.makedirs(STATE_DIR, exist_ok=True)
    try:
        os.unlink(SOCK_PATH)
    except FileNotFoundError:
        pass

    pool_size = DEFAULT_POOL_SIZE
    work_queue = queue.Queue()
    shutdown_event = threading.Event()
    last_activity = [time.monotonic()]
    activity_lock = threading.Lock()

    workers = []
    for _ in range(pool_size):
        t = threading.Thread(target=_worker_loop, args=(work_queue,))
        t.daemon = True
        t.start()
        workers.append(t)

    server = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    try:
        server.bind(SOCK_PATH)
    except OSError as e:
        print(
            "ocaml_server_shim: failed to bind socket %s: %s"
            % (SOCK_PATH, e),
            file=sys.stderr,
        )
        return
    server.listen()
    server.settimeout(0.5)

    with open(STATE_FILE, "w") as f:
        json.dump({"pid": os.getpid(), "socket": SOCK_PATH, "pool_size": pool_size}, f)

    if DEBUG:
        print(
            "ocaml_server_shim: listening on %s (pool_size=%d)"
            % (SOCK_PATH, pool_size),
            file=sys.stderr,
        )

    def handle_conn(conn):
        try:
            rfile = conn.makefile("rb")
            line = _recv_line(rfile)
            with activity_lock:
                last_activity[0] = time.monotonic()
            if line == b"SHUTDOWN\n":
                shutdown_event.set()
                return
            parts = line.decode("ascii", errors="replace").strip().split()
            if len(parts) != 3 or parts[0] != "REQ":
                return
            cwd_line = _recv_line(rfile)
            argc = int(parts[2])
            if argc < 0:
                return
            arg_lines = []
            for _ in range(argc):
                arg_lines.append(_recv_line(rfile))
            payload = b"".join(arg_lines)
            work_queue.put((line, cwd_line, payload, conn))
            conn = None
        except EOFError:
            pass
        finally:
            if conn is not None:
                conn.close()

    try:
        idle_timeout = DEFAULT_IDLE_TIMEOUT * 60.0
        while not shutdown_event.is_set():
            try:
                conn, _addr = server.accept()
            except socket.timeout:
                if idle_timeout > 0:
                    with activity_lock:
                        idle_for = time.monotonic() - last_activity[0]
                    if idle_for > idle_timeout:
                        shutdown_event.set()
                continue
            t = threading.Thread(target=handle_conn, args=(conn,))
            t.daemon = True
            t.start()
    finally:
        server.close()
        for _ in workers:
            work_queue.put(None)
        for t in workers:
            t.join()
        try:
            os.unlink(STATE_FILE)
        except FileNotFoundError:
            pass
        try:
            os.unlink(SOCK_PATH)
        except FileNotFoundError:
            pass


def _shutdown():
    try:
        sock = _connect_socket(SOCK_PATH, timeout=START_TIMEOUT)
        sock.sendall(b"SHUTDOWN\n")
        sock.close()
    except OSError:
        pass
    return 0


def main():
    if "--shutdown" in argv_rest:
        return _shutdown()
    _daemon_loop()
    return 0


if __name__ == "__main__":
    sys.exit(main())
