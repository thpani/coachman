# update opam & install ocamlfind (in opam)
FROM ocaml/opam
RUN cd opam-repository && git pull
RUN opam update
RUN opam install ocamlfind

# update apt + install binutils for gprof
RUN sudo apt-get update && sudo apt-get install -y binutils

# install codscost
WORKDIR /home/opam
ARG CACHEBUST=1
RUN opam pin add -n codscost https://github.com/thpani/codscost.git
RUN opam depext codscost
RUN opam install codscost
