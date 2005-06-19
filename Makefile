.PHONY: all lisp examples doc clean realclean distclean fullclean install test dist
.PRECIOUS: %.info %.html

include Makefile.defs

SUBDIRS = lisp examples

all: lisp muse.info

lisp:
	(cd lisp && $(MAKE))

examples:
	(cd examples && $(MAKE))

%.info: %.texi
	makeinfo muse.texi

%.html: %.texi
	makeinfo --html --no-split muse.texi

doc: muse.info muse.html

clean:
	for i in $(SUBDIRS); do \
	 (cd $$i && $(MAKE) clean); done

realclean distclean fullclean: clean
	-rm -f muse.info muse.html
	for i in $(SUBDIRS); do \
	 (cd $$i && $(MAKE) distclean); done

install: lisp muse.info
	(cd lisp && $(MAKE) install)
	install -d $(INFODIR)
	install -m 0644 muse.info $(INFODIR)/muse
	install-info --section "Emacs" "emacs" --info-dir=$(INFODIR) $(INFODIR)/muse

test: 
	(cd lisp && $(MAKE) test)

dist: distclean
	(cd ..; tar cvzf ~/Public/Emacs/muse.tar.gz muse)
