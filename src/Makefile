-include Makefile.common

OCAMLBUILD_TARGETS := \
  vocal.cma \
  vocal.cmxa \

INSTALLED_FILES := \
  _build/vocal.cmi \
  _build/vocal.cmx \
  _build/vocal.o \
  _build/vocal.cma \
  _build/vocal.cmxa \
  _build/vocal.a \

VOCAL_LIBS := $(shell cat vocal.mllib)

.PHONY: all install remove clean test wc tests export

all:
	@ ocamlbuild $(OCAMLBUILD_FLAGS) $(OCAMLBUILD_TARGETS)

# vocal.mlpack: $(wildcard *.ml)
#	@ echo $(filter-out test, $(subst .ml,,$^)) > $@

install: all
	ocamlfind install vocal META $(INSTALLED_FILES)

remove:
	ocamlfind remove vocal

clean:
	@ ocamlbuild $(OCAMLBUILD_FLAGS) -clean
	@ rm -f *~

docs:
	@ mkdir -p _build/doc
	@ ocamldoc -html -d _build/doc -I _build $(VOCAL_LIBS:%=%.ml)

tests:
	@ for f in tests/test_*.ml; do \
	    base=`basename $$f .ml`; \
	    ocamlbuild $(OCAMLBUILD_FLAGS) -pkg unix -pkg zarith \
		tests/$$base.native; \
	    ./$$base.native; \
	  done

wc:
	@ ocamlwc -p *.mli *.ml
