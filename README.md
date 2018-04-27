# CODSCOST

CODSCOST is a COmplexity Analyzer for Concurrent Heap-MANipulating programs.

## Run

The easiest way to get `codscost` running is in a Docker container – other build options are [listed below](#build).

```bash
$ docker run -it thpani/codscost
```

### Case studies

Several case studies are provided in directory `test/e2e`.
For example, run the following to obtain complexity results for Treiber's stack:

```bash
$ cd test/e2e/treiber
$ codscost treiber.tiny treiber.heap treiber.summaries
```

## Build

### Docker

```bash
$ git clone https://github.com/thpani/codscost.git codscost
$ docker build -t codscost codscost
$ docker run -it codscost
```

### Linux / MacOS

We assume you have a recent version of OCaml and opam installed.

```bash
$ git clone https://github.com/thpani/codscost.git codscost
$ opam pin add -n codscost codscost
$ opam depext -i codscost
$ export LD_LIBRARY_PATH="$(ocamlfind printconf destdir)/z3:${LD_LIBRARY_PATH}"
```