.PHONY: all lisp autoloads clean realclean distclean fullclean install test
.PRECIOUS: %.elc

include ../Makefile.defs

EL  = $(filter-out $(PROJECT)-autoloads.el,$(wildcard *.el))
ELC = $(patsubst %.el,%.elc,$(EL))

all: lisp

lisp: autoloads $(ELC)

$(PROJECT)-build.elc: ../scripts/$(PROJECT)-build.el
	@echo $(PROJECT)-build.el is not byte-compiled

autoloads: $(PROJECT)-autoloads.el

$(PROJECT)-autoloads.el: $(EL)
	@$(EMACS) -q $(SITEFLAG) -batch -l ../scripts/$(PROJECT)-build.el \
		-f $(PROJECT)-generate-autoloads . ../contrib ../experimental

%.elc: %.el
	@$(EMACS) -q $(SITEFLAG) -batch -l ../scripts/$(PROJECT)-build.el \
		-f batch-byte-compile $<

clean distclean:
	-rm -f *.elc *~

realclean fullclean: clean
	-rm -f $(PROJECT)-autoloads.el

install: autoloads $(ELC)
	install -d $(ELISPDIR)
	install -m 0644 $(PROJECT)-autoloads.el $(EL) $(ELC) $(ELISPDIR)

test: $(ELC)
	$(EMACS) -q $(SITEFLAG) -batch -l ../scripts/$(PROJECT)-build.el \
		-f $(PROJECT)-elint-files $(EL)
