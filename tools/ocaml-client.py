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


TOOL_NAME = os.path.basename(sys.argv[0])
STATE_DIR = os.path.join(tempfile.gettempdir(), "ocaml-server", TOOL_NAME)
STATE_FILE = os.path.join(STATE_DIR, "state.json")
SOCK_PATH = os.path.join(STATE_DIR, "sock")
LOCK_FILE = os.path.join(STATE_DIR, "lock")
LOG_FILE = os.path.join(STATE_DIR, "log")
START_TIMEOUT = 5.0
DEFAULT_POOL_SIZE = int(os.environ.get("OCAML_SERVER_THREADS", "1"))
DEFAULT_IDLE_TIMEOUT = int(os.environ.get("OCAML_SERVER_IDLE_MINUTES", "1"))
DEBUG = os.environ.get("OCAML_SERVER_DEBUG", "") != ""

shim_path = os.path.abspath(sys.argv[0])
shim_dir = os.path.dirname(shim_path)
candidate_orig = os.path.join(shim_dir, TOOL_NAME + ".orig")
candidate_same = os.path.join(shim_dir, TOOL_NAME)
if os.path.exists(candidate_orig):
    REAL_BIN = candidate_orig
elif os.path.exists(candidate_same) and os.path.abspath(candidate_same) != shim_path:
    REAL_BIN = candidate_same
else:
    REAL_BIN = TOOL_NAME

def _read_json(path):
    try:
        with open(path, "r") as f:
            return json.load(f)
    except FileNotFoundError:
        return None


def _write_json(path, data):
    tmp = path + ".tmp"
    with open(tmp, "w") as f:
        json.dump(data, f)
    os.replace(tmp, path)


def _acquire_lock():
    os.makedirs(STATE_DIR, exist_ok=True)
    while True:
        try:
            fd = os.open(LOCK_FILE, os.O_CREAT | os.O_EXCL | os.O_RDWR)
            os.close(fd)
            return
        except OSError as e:
            if e.errno != errno.EEXIST:
                raise
            time.sleep(0.05)


def _release_lock():
    try:
        os.unlink(LOCK_FILE)
    except FileNotFoundError:
        pass


def _connect_socket(path, timeout=0.25):
    s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    s.settimeout(timeout)
    s.connect(path)
    s.settimeout(None)
    return s


def _log_line(message):
    try:
        os.makedirs(STATE_DIR, exist_ok=True)
        with open(LOG_FILE, "ab", buffering=0) as f:
            line = "[%s] %s\n" % (time.strftime("%Y-%m-%d %H:%M:%S"), message)
            f.write(line.encode("utf-8", errors="replace"))
    except OSError:
        pass


def _server_running():
    state = _read_json(STATE_FILE)
    if not state:
        return False
    pid = state.get("pid")
    if not pid:
        return False
    try:
        os.kill(pid, 0)
    except OSError:
        return False
    try:
        s = _connect_socket(state.get("socket", SOCK_PATH))
        s.close()
        return True
    except OSError:
        return False


def _start_daemon():
    if DEBUG:
        print("ocaml_server_shim: starting daemon", file=sys.stderr)
    _acquire_lock()
    try:
        if _server_running():
            if DEBUG:
                print("ocaml_server_shim: daemon already running", file=sys.stderr)
            return
        cmd = [sys.executable, os.path.abspath(__file__), "--daemon"]
        os.makedirs(STATE_DIR, exist_ok=True)
        log = open(LOG_FILE, "ab", buffering=0)
        subprocess.Popen(
            cmd,
            stdin=subprocess.DEVNULL,
            stdout=log,
            stderr=log,
            start_new_session=True,
        )
        if DEBUG:
            print("ocaml_server_shim: daemon process spawned", file=sys.stderr)
        deadline = time.time() + START_TIMEOUT
        while time.time() < deadline:
            if _server_running():
                if DEBUG:
                    print("ocaml_server_shim: daemon is ready", file=sys.stderr)
                return
            time.sleep(0.05)
        raise RuntimeError("ocamlc server daemon did not start")
    finally:
        _release_lock()


def _recv_line(stream):
    line = stream.readline()
    if not line:
        raise EOFError
    return line


def _read_response(stream):
    line = _recv_line(stream)
    parts = line.decode("ascii", errors="replace").strip().split()
    if parts[:1] != ["RES"] or len(parts) != 5:
        raise RuntimeError("bad response header: %r" % line)
    out_len = int(parts[3])
    err_len = int(parts[4])
    data = stream.read(out_len + err_len)
    if len(data) != out_len + err_len:
        raise EOFError
    return line, data[:out_len], data[out_len:]


def _send_request(argv):
    if not _server_running():
        _start_daemon()
    state = _read_json(STATE_FILE)
    sock_path = state.get("socket", SOCK_PATH)
    sock = _connect_socket(sock_path, timeout=START_TIMEOUT)
    try:
        payload = b"\0".join(os.fsencode(a) for a in argv)
        req_id = "shim-%d" % os.getpid()
        header = ("REQ %s %d\n" % (req_id, len(payload))).encode("ascii")
        cwd_line = ("CWD %s\n" % os.getcwd()).encode(
            "utf-8", errors="surrogateescape"
        )
        sock.sendall(header + cwd_line + payload)
        rfile = sock.makefile("rb")
        try:
            line, out_data, err_data = _read_response(rfile)
        except EOFError as e:
            raise RuntimeError("server closed connection unexpectedly") from e
        parts = line.decode("ascii", errors="replace").strip().split()
        code = int(parts[2])
        sys.stdout.buffer.write(out_data)
        sys.stderr.buffer.write(err_data)
        sys.stdout.buffer.flush()
        sys.stderr.buffer.flush()
        return code
    finally:
        sock.close()


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

    _write_json(STATE_FILE, {"pid": os.getpid(), "socket": SOCK_PATH, "pool_size": pool_size})
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
            if not cwd_line.startswith(b"CWD ") or not cwd_line.endswith(b"\n"):
                _log_line("invalid cwd header from client: %r" % cwd_line)
                return
            length = int(parts[2])
            payload = rfile.read(length)
            if len(payload) != length:
                return
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
    state = _read_json(STATE_FILE)
    if not state:
        return 0
    sock_path = state.get("socket", SOCK_PATH)
    try:
        sock = _connect_socket(sock_path, timeout=START_TIMEOUT)
        sock.sendall(b"SHUTDOWN\n")
        sock.close()
    except OSError:
        pass
    return 0


def main():
    if len(sys.argv) > 1 and sys.argv[1] == "--print-request":
        payload = b"\0".join(os.fsencode(a) for a in sys.argv[2:])
        header = ("REQ t1 %d\n" % len(payload)).encode("ascii")
        cwd_line = ("CWD %s\n" % os.getcwd()).encode(
            "utf-8", errors="surrogateescape"
        )
        sys.stdout.buffer.write(header + cwd_line + payload)
        return 0
    if len(sys.argv) > 1 and sys.argv[1] == "--daemon":
        _daemon_loop()
        return 0
    if len(sys.argv) > 1 and sys.argv[1] == "--shutdown":
        return _shutdown()
    return _send_request(sys.argv[1:])


if __name__ == "__main__":
    sys.exit(main())
