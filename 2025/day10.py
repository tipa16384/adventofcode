import socket
from z3 import *

HOST = "127.0.0.1"
PORT = 5000

def recv_line(conn):
    chunks = []
    while True:
        data = conn.recv(4096)
        if not data:
            # connection closed
            break
        chunks.append(data)
        if b"\n" in data:
            break

    if not chunks:
        return ""
    buf = b"".join(chunks)
    line, _, _rest = buf.partition(b"\n")
    return line.decode("utf-8")

def send_line(conn, text):
    if not text.endswith("\n"):
        text = text + "\n"
    conn.sendall(text.encode("utf-8"))

def handle_request(text: str) -> str:
    splits = text.strip().split()
    solve_values = list(map(int, splits[-1][1:-1].split(",")))
    operands = [list(map(int, x[1:-1].split(","))) for x in splits[1:-1]]
    s = Optimize()
    vars = [Int(f"x{i}") for i in range(len(operands) + 1)]
    m = Int("m")
    for v in vars: s.add(v >= 0)
    eval('s.add(m == ' + ' + '.join("vars[{}]".format(i) for i in range(len(operands))) + ')')
    for i in range(len(solve_values)):
        solve_for = solve_values[i]
        terms = 's.add(' + str(solve_for) + ' == ' + \
            ' + '.join(["vars[{}]".format(j) for j in range(len(operands)) if i in operands[j]]) + ')'
        eval(terms)
    h = s.minimize(m)
    if s.check() == sat:
        print (s.model())
        return str(h.value())
    return None

def main():
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as server:
        server.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        server.bind((HOST, PORT))
        server.listen()
        print(f"Listening on {HOST}:{PORT}")

        while True:
            conn, addr = server.accept()
            print("Connection from", addr)
            with conn:
                request = recv_line(conn)
                if request == "":
                    print("Empty or closed connection")
                    continue

                print("Request:", repr(request))
                response = handle_request(request)
                print("Response:", repr(response))

                send_line(conn, response)

if __name__ == "__main__":
    main()
