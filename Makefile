.PHONY: all clean native sanity unit test

OCB_FLAGS = -use-ocamlfind -I src -I src/programs -I src/heaps
OCB = ocamlbuild $(OCB_FLAGS)

TREIBER := testt testt_emp testt_pop testt_push

all: native

clean:
	$(OCB) -clean

native: sanity
	$(OCB) main.native

# check that packages can be found
sanity:
	ocamlfind query ocamlgraph

unit:
	$(OCB) -package oUnit -I test/unit sca.native
	./sca.native

$(TREIBER): native
	@$(MAKE) -C test/e2e/treiber $(patsubst _%,%,$(patsubst testt%,%,$@))

testms: native
	@$(MAKE) -C test/e2e/ms
