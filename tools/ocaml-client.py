#!/usr/bin/env python3
import errno
import json
import os
import socket
import subprocess
import sys
import tempfile
import time


TOOL_NAME = os.path.basename(sys.argv[0])
DEFAULT_STATE_DIR = os.path.join(
    tempfile.gettempdir(), "ocaml_server_shim", TOOL_NAME
)
STATE_DIR = os.environ.get("OCAML_SERVER_TMPDIR", DEFAULT_STATE_DIR)
STATE_FILE = os.path.join(STATE_DIR, "state.json")
SOCK_PATH = os.path.join(STATE_DIR, "%s.sock" % TOOL_NAME)
LOCK_FILE = os.path.join(STATE_DIR, "lock")
LOG_FILE = os.path.join(STATE_DIR, "daemon.log")
START_TIMEOUT = 5.0
DEBUG = os.environ.get("OCAML_SERVER_DEBUG", "") != ""


shim_path = os.path.realpath(sys.argv[0])
shim_dir = os.path.dirname(shim_path)
invoked_path = os.path.abspath(sys.argv[0])
invoked_dir = os.path.dirname(invoked_path)
SERVER_BIN = os.path.join(shim_dir, "ocaml-server.py")


def _compute_real_bin():
    if os.environ.get("OCAML_SERVER_REAL_BIN"):
        return os.environ.get("OCAML_SERVER_REAL_BIN")
    if os.environ.get("OCAML_SERVER_BIN"):
        return os.environ.get("OCAML_SERVER_BIN")
    candidate_orig = os.path.join(invoked_dir, TOOL_NAME + ".orig")
    candidate_opt_orig = os.path.join(invoked_dir, TOOL_NAME + ".opt.orig")
    candidate_same = os.path.join(invoked_dir, TOOL_NAME)
    if os.path.exists(candidate_orig):
        return candidate_orig
    if os.path.exists(candidate_opt_orig):
        return candidate_opt_orig
    if os.path.exists(candidate_same) and os.path.abspath(candidate_same) != invoked_path:
        return candidate_same
    return TOOL_NAME


def _read_json(path):
    try:
        with open(path, "r") as f:
            return json.load(f)
    except FileNotFoundError:
        return None


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


def _tail_log(path, max_bytes=8192):
    try:
        with open(path, "rb") as f:
            try:
                f.seek(-max_bytes, os.SEEK_END)
            except OSError:
                f.seek(0)
            data = f.read().decode("utf-8", errors="replace")
            return data.strip()
    except FileNotFoundError:
        return ""


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
        if not os.path.exists(SERVER_BIN):
            raise RuntimeError("server binary not found: %s" % SERVER_BIN)
        cmd = [sys.executable, SERVER_BIN, "--daemon", "--tool", TOOL_NAME]
        env = os.environ.copy()
        if "OCAML_SERVER_REAL_BIN" not in env and "OCAML_SERVER_BIN" not in env:
            env["OCAML_SERVER_REAL_BIN"] = _compute_real_bin()
        os.makedirs(STATE_DIR, exist_ok=True)
        log = open(LOG_FILE, "ab", buffering=0)
        subprocess.Popen(
            cmd,
            stdin=subprocess.DEVNULL,
            stdout=log,
            stderr=log,
            start_new_session=True,
            env=env,
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
        tail = _tail_log(LOG_FILE)
        msg = "server daemon did not start"
        if tail:
            msg += "\n\n" + tail
        raise RuntimeError(msg)
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
        req_id = "shim-%d" % os.getpid()
        header = ("REQ %s %d\n" % (req_id, len(argv))).encode("ascii")
        cwd_line = (os.getcwd() + "\n").encode(
            "utf-8", errors="surrogateescape"
        )
        arg_lines = b"".join(os.fsencode(a) + b"\n" for a in argv)
        sock.sendall(header + cwd_line + arg_lines)
        rfile = sock.makefile("rb")
        try:
            line, out_data, err_data = _read_response(rfile)
        except EOFError as e:
            tail = _tail_log(LOG_FILE)
            msg = "server closed connection unexpectedly"
            if tail:
                msg += "\n\n" + tail
            raise RuntimeError(msg) from e
        parts = line.decode("ascii", errors="replace").strip().split()
        code = int(parts[2])
        sys.stdout.buffer.write(out_data)
        sys.stderr.buffer.write(err_data)
        sys.stdout.buffer.flush()
        sys.stderr.buffer.flush()
        return code
    finally:
        sock.close()


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
        args = sys.argv[2:]
        header = ("REQ t1 %d\n" % len(args)).encode("ascii")
        cwd_line = (os.getcwd() + "\n").encode(
            "utf-8", errors="surrogateescape"
        )
        arg_lines = b"".join(os.fsencode(a) + b"\n" for a in args)
        sys.stdout.buffer.write(header + cwd_line + arg_lines)
        return 0
    if len(sys.argv) > 1 and sys.argv[1] == "--shutdown":
        return _shutdown()
    if len(sys.argv) > 1 and sys.argv[1] == "--daemon":
        cmd = [sys.executable, SERVER_BIN, "--daemon", "--tool", TOOL_NAME]
        env = os.environ.copy()
        if "OCAML_SERVER_REAL_BIN" not in env and "OCAML_SERVER_BIN" not in env:
            env["OCAML_SERVER_REAL_BIN"] = _compute_real_bin()
        os.execve(sys.executable, cmd, env)
    return _send_request(sys.argv[1:])


if __name__ == "__main__":
    sys.exit(main())
