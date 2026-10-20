# Copyright (C) 2003, 2004 Lars Brinkhoff.

EMACS = emacs
COMPILER ?= byte

all:
	EMACS_CL_COMPILER=$(COMPILER) $(EMACS) -Q -batch -l load-cl.el -f compile-cl

check:
	./test

clean:
	rm -f *.elc
	rm -rf .eln
