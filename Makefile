PYTHON	= python3
INT_DRIVER_GENERATOR	= $(CURDIR)/drivers/types/gen_int_driver.py

INT_DRIVERS = UInt4.drv UInt8.drv UInt160.drv UInt256.drv

default : evm.ml

drivers: $(addprefix $(CURDIR)/drivers/types/, $(INT_DRIVERS))

$(CURDIR)/drivers/types/UInt%.drv: $(INT_DRIVER_GENERATOR)
	$(PYTHON) $(INT_DRIVER_GENERATOR) $* > $@


evm.ml: $(CURDIR)/src/MinimalEVM.mlw drivers
	why3 extract MinimalEVM.EVM -D ocaml64 -L src -L lib \
		$(addprefix -D $(CURDIR)/drivers/types/, $(INT_DRIVERS)) \
		-D $(CURDIR)/drivers/types/int.drv \
		> $@

clean:
	rm $(addprefix $(CURDIR)/drivers/types/, $(INT_DRIVERS))
	rm evm.ml
