.PHONY: all clean native sanity unit test

OCB_FLAGS = -use-ocamlfind -I src -I src/programs -I src/heaps
OCB = ocamlbuild $(OCB_FLAGS)

TREIBER := testt_emp testt_pop testt_push

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

testt: native
	@$(MAKE) -C test/e2e/treiber

$(TREIBER): native
	@$(MAKE) -C test/e2e/treiber $(patsubst testt_%,%,$@)
