.PHONY: all clean native sanity unit testt testt_emp testt_push testt_pop

OCB_FLAGS = -use-ocamlfind -I src -I src/programs -I src/heaps
OCB = ocamlbuild $(OCB_FLAGS)

all: native

clean:
	$(OCB) -clean

native: sanity
	$(OCB) main.native

# check that packages can be found
sanity:
	ocamlfind query ocamlgraph

unit:
	$(OCB) -package oUnit -I test/unit heaptest.native
	./heaptest.native

testt_emp: native
	./main.native test/e2e/treiber/treiber.tiny test/e2e/treiber/treiber.heap test/e2e/empty.summaries

testt_pop: native
	./main.native test/e2e/treiber/treiber_pop.tiny test/e2e/treiber/treiber.heap test/e2e/treiber/treiber.summaries

testt_push: native
	./main.native test/e2e/treiber/treiber_push.tiny test/e2e/treiber/treiber.heap test/e2e/treiber/treiber.summaries

testt: testt_emp testt_pop testt_push
