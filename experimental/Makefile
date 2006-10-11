.PHONY: all experimental clean realclean distclean fullclean install
.PHONY: install-uncompiled
.PRECIOUS: %.elc

include ../Makefile.defs

EL  = $(wildcard *.el)
ELC = $(patsubst %.el,%.elc,$(wildcard *.el))

all: experimental

experimental: $(ELC)

$(PROJECT)-build.elc: ../scripts/$(PROJECT)-build.el
	@echo $(PROJECT)-build.el is not byte-compiled

%.elc: %.el
	@$(EMACS) -q $(SITEFLAG) -batch -l ../scripts/$(PROJECT)-build.el \
		-f batch-byte-compile $<

clean realclean distclean fullclean:
	-rm -f *.elc *~

install: $(ELC)
	install -d $(ELISPDIR)/experimental
	install -m 0644 $(EL) $(ELC) $(ELISPDIR)/experimental

install-uncompiled:
	install -d $(ELISPDIR)/experimental
	install -m 0644 $(EL) $(ELISPDIR)/experimental
