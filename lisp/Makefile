.PHONY: all lisp clean realclean distclean fullclean test
.PRECIOUS: %.elc

include ../Makefile.defs

ELC = $(patsubst %.el,%.elc,$(wildcard *.el))

all: lisp

lisp: $(ELC)

muse-build.elc: ../scripts/muse-build.el
	@echo muse-build.el is not byte-compiled

%.elc: %.el
	@$(EMACS) -q $(SITEFLAG) -batch \
		-L ../contrib -L .  \
		-l ../scripts/muse-build.el\
		-f batch-byte-compile $<

clean realclean distclean fullclean:
	-rm -f *.elc *~

test: $(ELC)
	$(EMACS) -q $(SITEFLAG) -batch \
		-L ../contrib -L .  \
		-l ../scripts/muse-build.el \
		-f muse-elint-files muse-*.el
