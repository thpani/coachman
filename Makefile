OCB_FLAGS = -use-ocamlfind
INCLUDES_SRC = -I src -I src/programs -I src/heaps
OCB = ocamlbuild $(OCB_FLAGS) $(INCLUDES_SRC)

.PHONY: all clean native unit e2e test_unit test_e2e test doc install uninstall

all: native

clean:
	$(OCB) -clean

native:
	$(OCB) coachman.native

unit:
	$(OCB) -I test/unit unit_all.native

e2e: 
	$(OCB) -I test/e2e e2e_all.native

test_unit: unit
	./unit_all.native

test_e2e: e2e
	./e2e_all.native

test: test_unit test_e2e

doc:
	$(OCB) doc/api.docdir/index.html
	$(OCB) doc/api.docdir/api.dot

install: native
	install -m 0755 coachman.native $(prefix)/bin/coachman

uninstall: native
	rm -f $(prefix)/bin/coachman
