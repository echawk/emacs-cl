COMPILER ?= byte

all check clean:
	$(MAKE) -C lisp $@ COMPILER=$(COMPILER)
