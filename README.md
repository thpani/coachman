# COACHMAN

COACHMAN is a COmplexity Analyzer for Concurrent Heap-MANipulating programs.

## Run

The easiest way to get `coachman` running is in a Docker container – other build options are [listed below](#build).

```bash
$ docker run -it thpani/coachman
```

### Case studies

Several case studies are provided in directory `test/e2e`.
For example, run the following to obtain complexity results for Treiber's stack:

```bash
$ cd test/e2e/treiber
$ coachman treiber.tiny treiber.heap treiber.summaries
```

## Build

### Docker

```bash
$ git clone https://github.com/thpani/coachman.git coachman
$ docker build -t coachman coachman
$ docker run -it coachman
```

### Linux / MacOS

We assume you have a recent version of OCaml and opam installed.

```bash
$ git clone https://github.com/thpani/coachman.git coachman
$ opam pin add -n coachman coachman
$ opam depext -i coachman
$ export LD_LIBRARY_PATH="$(ocamlfind printconf destdir)/z3:${LD_LIBRARY_PATH}"
```
