# Copyright (C) 2003, 2004 Lars Brinkhoff.

EMACS = emacs

all:
	$(EMACS) -batch -l load-cl.el -f compile-cl

check:
	bash test

clean:
	rm -f *.elc
