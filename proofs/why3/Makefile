.PHONY: replay

SRC=../../src/

WHY3=Vector UnionFind PriorityQueueVector Arrays ZipperList PairingHeap Queue

WHY3ML=$(addsuffix .ml, $(WHY3))
WHY3IMPL=$(addsuffix _impl, $(WHY3))
WHY3IMPLMLW=$(addsuffix .mlw, $(WHY3IMPL))

WHY3GEN=$(addprefix $(SRC), $(WHY3ML))

all: $(WHY3GEN)
	make -C ../..

%.ide:
	why3 ide -L . -L $(SRC) $*.mlw
%.prove:
	why3 prove -L . -L $(SRC) $*.mlw

WHY3EXTRACT=why3 extract -D ocaml64 -D uf.drv\
		--modular --recursive-deps -D queue.drv

WHY3EXTRACTNR=why3 extract -D ocaml64 -D uf.drv\
		--modular

$(SRC)PriorityQueueVector.ml : PriorityQueueVector_impl.mlw $(SRC)PriorityQueueVector.mli
	$(WHY3EXTRACTNR)  -L . -L $(SRC) -o $(SRC) PriorityQueueVector_impl.Impl
	sed -e "s/__Sig//g" $(SRC)PriorityQueueVector_impl__Impl.ml > $@
	rm -f $(SRC)PriorityQueueVector_impl__Impl.ml
	if test -f $(SRC)PriorityQueueVector_impl.mli.equiv; then \
	  echo ""  >> $@; \
	  cat $(SRC)PriorityQueueVector_impl.mli.equiv >> $@; \
	fi
	if test -f $(SRC)PriorityQueueVector_impl.ml.unproved; then \
	  echo ""  >> $@; \
	  cat $(SRC)PriorityQueueVector_impl.ml.unproved >> $@; \
	fi

$(SRC)%.ml: %_impl.mlw $(SRC)%.mli
	$(WHY3EXTRACT)  -L . -L $(SRC) -o $(SRC) $*_impl.Impl
	sed -e "s/__Sig//g" $(SRC)$*_impl__Impl.ml > $@
	rm -f $(SRC)$*_impl__Impl.ml
	if test -f $(SRC)$*.mli.equiv; then \
	  echo ""  >> $@; \
	  cat $(SRC)$*.mli.equiv >> $@; \
	fi
	if test -f $(SRC)$*.ml.unproved; then \
	  echo ""  >> $@; \
	  cat $(SRC)$*.ml.unproved >> $@; \
	fi

uf.ml: uf.drv UnionFind_impl.mlw
	why3 extract -D ocaml64 -D uf.drv -L . -L $(SRC) \
	UnionFind_impl.Impl -o $@

test_uf: uf.ml test_uf.ml
	ocamlopt uf.ml test_uf.ml -o $@
	./$@

maze.native: uf.ml maze.ml
	ocamlbuild -package graphics $@

replay:
	@$(foreach x, $(WHY3), echo "Replaying $x"; \
		why3 replay -L . -L $(SRC) $(x)_impl ;)
