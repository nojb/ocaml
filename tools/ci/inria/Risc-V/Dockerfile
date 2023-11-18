FROM riscv64/ubuntu:latest

RUN apt-get update && apt-get install -y gcc make git

CMD cd /root/ocaml && tools/ci/inria/main
