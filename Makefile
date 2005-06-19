.PHONY: all lisp examples doc clean realclean distclean fullclean test dist

include Makefile.defs

SUBDIRS = lisp examples

all: lisp

lisp:
	(cd lisp && $(MAKE))

examples:
	(cd examples && $(MAKE))

doc:
	makeinfo muse.texi
	makeinfo --html --no-split muse.texi

clean:
	for i in $(SUBDIRS); do \
	 (cd $$i && $(MAKE) clean); done

realclean distclean fullclean: clean
	-rm -f muse.info muse.html
	for i in $(SUBDIRS); do \
	 (cd $$i && $(MAKE) distclean); done

test: 
	(cd lisp && $(MAKE) test)

dist: distclean
	(cd ..; tar cvzf ~/Public/Emacs/muse.tar.gz muse)
