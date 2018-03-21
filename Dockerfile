# update opam & install ocamlfind (in opam)
FROM ocaml/opam
RUN cd opam-repository && git pull
RUN opam update
RUN opam install ocamlfind

# install z3
RUN sudo apt-get update && \
    sudo apt-get install -y python
RUN git clone https://github.com/Z3Prover/z3.git
WORKDIR /home/opam/z3
RUN git checkout z3-4.6.0
RUN opam config exec -- python scripts/mk_make.py --ml
RUN cd build && opam config exec -- make
RUN cd build && opam config exec -- sudo make install

# install codscost
WORKDIR /home/opam
ARG CACHEBUST=1
RUN git clone https://github.com/thpani/codscost.git
RUN opam pin add -n codscost /home/opam/codscost
RUN opam depext codscost
RUN opam install codscost

# install binutils for gprof
RUN sudo apt-get install -y binutils
