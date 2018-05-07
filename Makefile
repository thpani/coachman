OCB_FLAGS = -use-ocamlfind -I src -I src/programs -I src/heaps -I test -I test/e2e -I test/unit
OCB = ocamlbuild $(OCB_FLAGS)

.PHONY: all clean native unit test_unit e2e test_e2e testall test_all doc install testclean $(TREIBER) $(MS)

all: native

clean:
	$(OCB) -clean

native:
	$(OCB) coachman.native

unit:
	$(OCB) unit_all.native

test_unit: unit
	./unit_all.native

e2e: 
	$(OCB) e2e_all.native

test_e2e: e2e
	./e2e_all.native

testall:
	$(OCB) test_all.native

test_all: testall
	./test_all.native

doc:
	$(OCB) doc/api.docdir/index.html
	$(OCB) doc/api.docdir/api.dot

install: native
	install -m 0755 coachman.native $(prefix)/bin/coachman
