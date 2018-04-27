FROM ocaml/opam

# update opam
RUN cd opam-repository && git pull
RUN opam update

# update apt
RUN sudo apt-get update

# install codscost
ARG CACHEBUST=1
RUN git clone https://github.com/thpani/codscost.git
RUN opam pin add -n codscost codscost
RUN opam depext codscost
RUN opam install codscost
ENV LD_LIBRARY_PATH /home/opam/.opam/4.05.0/lib/z3:$LD_LIBRARY_PATH
WORKDIR /home/opam/codscost
