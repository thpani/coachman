# Coachman [![Build Status](https://travis-ci.org/thpani/coachman.svg?branch=master)](https://travis-ci.org/thpani/coachman)

<img align="right" width="150" src="https://user-images.githubusercontent.com/82047/39621966-d8711dec-4f90-11e8-9792-7c31968157c6.jpg">
Coachman is a COmplexity Analyzer for Concurrent Heap-MANipulating programs.

It implements

* translation from heap-manipulating programs to integer programs from [[1]](#references) (slightly extended for atomic transitions)
* rely-guarantee bound analysis from [[2]](#references)
* sequential bound analysis based on [[3]](#references)

## Run

The easiest way to get `coachman` running is in a Docker container – other build options are [listed below](#build).

```bash
$ docker pull thpani/coachman
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

# References

[1] A. Bouajjani, M. Bozga, P. Habermehl, R. Iosif, P. Moro, and T. Vojnar, “Programs with lists are counter automata,” Formal Methods in System Design, vol. 38, no. 2, pp. 158–192, 2011.  
[2] T. Pani, G. Weissenbacher, F. Zuleger, “Rely-Guarantee Reasoning for Automated Complexity Analysis of Non-Blocking Algorithms,” Note: under submission.  
[3] M. Sinn, F. Zuleger, and H. Veith, “Complexity and resource bound analysis of imperative programs using difference constraints,” J. Autom. Reasoning, vol. 59, no. 1, pp. 3–45, 2017.
