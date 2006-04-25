.PHONY: all contrib clean realclean distclean fullclean install
.PRECIOUS: %.elc

include ../Makefile.defs

EL  = $(wildcard *.el)
ELC = $(patsubst %.el,%.elc,$(EL))

all: contrib

contrib: $(ELC)

muse-build.elc: ../scripts/muse-build.el
	@echo muse-build.el is not byte-compiled

%.elc: %.el
	@$(EMACS) -q $(SITEFLAG) -batch -l ../scripts/muse-build.el \
		-f batch-byte-compile $<

clean realclean distclean fullclean:
	-rm -f *.elc *~

install: $(ELC)
	install -d $(ELISPDIR)/contrib
	install -m 0644 $(EL) $(ELC) $(ELISPDIR)/contrib
