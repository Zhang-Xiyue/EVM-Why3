PYTHON=python3
INT_DRIVER_GENERATOR=$(CURDIR)/drivers/types/gen_int_driver.py
INT_DRIVERS=UInt4.drv UInt8.drv UInt160.drv UInt256.drv
OCAML_SOURCES=evm.ml common.ml
OCAML_TARGETS=$(patsubst %.ml,%.cmo,$(OCAML_SOURCES))

default : evm.ml

drivers: $(addprefix $(CURDIR)/drivers/types/, $(INT_DRIVERS))

$(CURDIR)/drivers/types/UInt%.drv: $(INT_DRIVER_GENERATOR)
	$(PYTHON) $(INT_DRIVER_GENERATOR) $* > $@


evm.ml: $(CURDIR)/src/MinimalEVM.mlw drivers $(CURDIR)/src/ocaml/dependency.ml
	cp $(CURDIR)/src/ocaml/dependency.ml $@
	why3 extract MinimalEVM.EVM -L src -L lib \
		$(addprefix -D $(CURDIR)/drivers/types/, $(INT_DRIVERS)) \
		-D $(CURDIR)/drivers/ocaml64.drv \
		-D $(CURDIR)/drivers/types/int.drv \
		>> $@

%.cmo: %.ml
	ocamlc -c $< -o $@

server: $(OCAML_TARGETS) server.ml
	ocamlfind ocamlc -linkpkg -package num $< -o $@

test: $(OCAML_TARGETS) test.cmo
	ocamlfind ocamlc -linkpkg -package num $^ -o $@

run: evm
	./evm

clean:
	rm -f $(addprefix $(CURDIR)/drivers/types/, $(INT_DRIVERS))
	rm -f evm.ml.in
	rm -f evm.ml
	rm -f evm test server
	rm -f *.cmo
	rm -f *.cmi
	rm -f *.cmx
	rm -f *.o
