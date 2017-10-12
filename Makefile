#
# Pure OCaml, package from Opam, two directories
#

# - The -I flag introduces sub-directories
# - -use-ocamlfind is required to find packages (from Opam)
# - _tags file introduces packages, bin_annot flag for tool chain

.PHONY: all clean byte native profile debug sanity test test1 test2 test3 testt testt_emp unit

OCB_FLAGS = -use-ocamlfind -I src -I src/programs -I src/heaps -I lib
OCB = ocamlbuild $(OCB_FLAGS)

all: native byte # profile debug

clean:
	$(OCB) -clean

native: sanity
	$(OCB) main.native

byte: sanity
	$(OCB) main.byte

profile: sanity
	$(OCB) -tag profile main.native

debug: sanity
	$(OCB) -tag debug main.byte

# check that packages can be found
sanity:
	ocamlfind query ocamlgraph

test: native
	./main.native test.tiny empty.heap
	dot -Tpdf test.tiny.dot >test.tiny.dot.pdf
	open test.tiny.dot.pdf
	dot -Tpdf test.tiny.bi.dot >test.tiny.bi.dot.pdf
	open test.tiny.bi.dot.pdf

test2: native
	./main.native test2.tiny empty.heap
	dot -Tpdf test2.tiny.dot >test2.tiny.dot.pdf
	open test2.tiny.dot.pdf
	dot -Tpdf test2.tiny.bi.dot >test2.tiny.bi.dot.pdf
	open test2.tiny.bi.dot.pdf

test3: native
	./main.native test3.tiny empty.heap
	dot -Tpdf test3.tiny.dot >test3.tiny.dot.pdf
	open test3.tiny.dot.pdf
	dot -Tpdf test3.tiny.bi.dot >test3.tiny.bi.dot.pdf
	open test3.tiny.bi.dot.pdf

testt: native
	./main.native treiber.tiny treiber.heap treiber.summaries
	dot -Tpdf treiber.tiny.dot >treiber.tiny.dot.pdf
	open treiber.tiny.dot.pdf
	dot -Tpdf treiber.tiny.bi.dot >treiber.tiny.bi.dot.pdf
	open treiber.tiny.bi.dot.pdf

testt_emp: native
	./main.native treiber.tiny treiber.heap empty.summaries
	dot -Tpdf treiber.tiny.dot >treiber.tiny.dot.pdf
	open treiber.tiny.dot.pdf
	dot -Tpdf treiber.tiny.bi.dot >treiber.tiny.bi.dot.pdf
	open treiber.tiny.bi.dot.pdf

unit:
	$(OCB) -package oUnit -I test/unit heaptest.native
	./heaptest.native
