.PHONY: all lisp clean realclean distclean fullclean install test
.PRECIOUS: %.elc

include ../Makefile.defs

EL  = $(wildcard *.el)
ELC = $(patsubst %.el,%.elc,$(wildcard *.el))

all: lisp

lisp: $(ELC)

muse-build.elc: ../scripts/muse-build.el
	@echo muse-build.el is not byte-compiled

%.elc: %.el
	@$(EMACS) -q $(SITEFLAG) -batch -l ../scripts/muse-build.el \
		-f batch-byte-compile $<

clean realclean distclean fullclean:
	-rm -f *.elc *~

install: $(ELC)
	install -d $(ELISPDIR)
	install -m 0644 $(EL) $(ELC) $(ELISPDIR)

test: $(ELC)
	$(EMACS) -q $(SITEFLAG) -batch -l ../scripts/muse-build.el \
		-f muse-elint-files muse-*.el
