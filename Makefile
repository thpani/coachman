#
# Pure OCaml, package from Opam, two directories
#

# - The -I flag introduces sub-directories
# - -use-ocamlfind is required to find packages (from Opam)
# - _tags file introduces packages, bin_annot flag for tool chain

.PHONY: all clean byte native profile debug sanity unit testt testt_emp testt_push testt_pop dot_testt testms testms_emp testms_push testms_pop dot_testms

OCB_FLAGS = -use-ocamlfind -use-menhir -I src -I src/programs -I src/heaps -I lib
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

unit:
	$(OCB) -package oUnit -I test/unit heaptest.native
	./heaptest.native

testt_emp: native
	./main.native test/e2e/treiber.tiny test/e2e/treiber.heap test/e2e/empty.summaries

testt_pop: native
	./main.native test/e2e/treiber.tiny test/e2e/treiber.heap test/e2e/treiber_pop.summaries

testt_push: native
	./main.native test/e2e/treiber.tiny test/e2e/treiber.heap test/e2e/treiber_push.summaries

testt_push_pop: native
	./main.native test/e2e/treiber.tiny test/e2e/treiber.heap test/e2e/treiber.summaries

testt: testt_emp testt_pop testt_push testt_push_pop

dot_testt:
	dot -Tpdf treiber.tiny.push.dot >treiber.tiny.push.dot.pdf && open treiber.tiny.push.dot.pdf
	dot -Tpdf treiber.tiny.push.bi.dot >treiber.tiny.push.bi.dot.pdf && open treiber.tiny.push.bi.dot.pdf
	dot -Tpdf treiber.tiny.pop.dot >treiber.tiny.pop.dot.pdf && open treiber.tiny.pop.dot.pdf
	dot -Tpdf treiber.tiny.pop.bi.dot >treiber.tiny.pop.bi.dot.pdf && open treiber.tiny.pop.bi.dot.pdf

testms_emp: native
	./main.native test/e2e/ms.tiny test/e2e/ms.heap test/e2e/empty.summaries

testms_enq: native
	./main.native test/e2e/ms.tiny test/e2e/ms.heap test/e2e/ms_enq.summaries

testms_deq: native
	./main.native test/e2e/ms.tiny test/e2e/ms.heap test/e2e/ms_deq.summaries

testms_enq_deq: native
	./main.native test/e2e/ms.tiny test/e2e/ms.heap test/e2e/ms.summaries

testms: testms_emp testms_enq testms_deq testms_enq_deq

dot_testms:
	dot -Tpdf ms.tiny.enq.dot >ms.tiny.enq.dot.pdf && open ms.tiny.enq.dot.pdf
	dot -Tpdf ms.tiny.enq.bi.dot >ms.tiny.enq.bi.dot.pdf && open ms.tiny.enq.bi.dot.pdf
	dot -Tpdf ms.tiny.deq.dot >ms.tiny.deq.dot.pdf && open ms.tiny.deq.dot.pdf
	dot -Tpdf ms.tiny.deq.bi.dot >ms.tiny.deq.bi.dot.pdf && open ms.tiny.deq.bi.dot.pdf
