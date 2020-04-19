PYTHON	= python3
INT_DRIVER_GENERATOR	= $(CURDIR)/drivers/types/gen_int_driver.py

INT_DRIVERS = UInt4.drv UInt8.drv UInt160.drv UInt256.drv

default : evm.ml

drivers: $(addprefix $(CURDIR)/drivers/types/, $(INT_DRIVERS))

$(CURDIR)/drivers/types/UInt%.drv: $(INT_DRIVER_GENERATOR)
	$(PYTHON) $(INT_DRIVER_GENERATOR) $* > $@


evm.ml: $(CURDIR)/src/MinimalEVM.mlw drivers
	cp $(CURDIR)/src/ocaml/EVMDependency.ml $@
	why3 extract MinimalEVM.EVM -L src -L lib \
		$(addprefix -D $(CURDIR)/drivers/types/, $(INT_DRIVERS)) \
		-D $(CURDIR)/drivers/ocaml64.drv \
		-D $(CURDIR)/drivers/types/int.drv \
		>> $@
	cat $(CURDIR)/src/ocaml/EVMServer.ml >> $@

evm: evm.ml
	ocamlfind ocamlopt -linkpkg -package num $< -o $@

run: evm
	./evm

clean:
	rm -f $(addprefix $(CURDIR)/drivers/types/, $(INT_DRIVERS))
	rm -f evm.ml.in
	rm -f evm.ml
	rm -f evm
