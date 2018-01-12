FROM ocaml/opam
RUN cd opam-repository && git pull
RUN opam update
RUN opam install ocamlfind

RUN sudo apt-get update && \
    sudo apt-get install -y python

RUN git clone https://github.com/Z3Prover/z3.git
WORKDIR /home/opam/z3
RUN git checkout z3-4.6.0
RUN opam config exec -- python scripts/mk_make.py --ml
RUN cd build && opam config exec -- make
RUN cd build && opam config exec -- sudo make install

WORKDIR /home/opam
RUN git clone https://github.com/thpani/codscost.git
RUN opam pin add -n codscost /home/opam/codscost
RUN opam depext codscost
RUN opam install codscost

# FROM debian:stretch
# RUN apt-get update && \
#     apt-get install -y binutils build-essential git ocaml libz3-ocaml-dev libapron-ocaml-dev libocamlgraph-ocaml-dev
# RUN git clone https://github.com/thpani/codscost.git
