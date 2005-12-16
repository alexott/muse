.PHONY: all lisp autoloads clean realclean distclean fullclean install test
.PRECIOUS: %.elc

include ../Makefile.defs

EL  = $(filter-out muse-autoloads.el,$(wildcard *.el))
ELC = $(patsubst %.el,%.elc,$(wildcard *.el))

all: autoloads lisp

lisp: $(ELC)

muse-build.elc: ../scripts/muse-build.el
	@echo muse-build.el is not byte-compiled

autoloads: muse-autoloads.el

muse-autoloads.el: $(EL)
	@$(EMACS) -q $(SITEFLAG) -batch -l ../scripts/muse-build.el \
		-f muse-generate-autoloads . ../contrib ../experimental

%.elc: %.el
	@$(EMACS) -q $(SITEFLAG) -batch -l ../scripts/muse-build.el \
		-f batch-byte-compile $<

clean:
	-rm -f *.elc *~

realclean distclean fullclean: clean
	-rm -f muse-autoloads.el

install: $(ELC)
	install -d $(ELISPDIR)
	install -m 0644 $(EL) $(ELC) $(ELISPDIR)

test: $(ELC)
	$(EMACS) -q $(SITEFLAG) -batch -l ../scripts/muse-build.el \
		-f muse-elint-files muse-*.el
