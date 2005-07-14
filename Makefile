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
	$(INSTALLINFO) $(INFODIR)/muse

test: 
	(cd lisp && $(MAKE) test)

dist: distclean ../muse-$(VERSION).zip ../muse-$(VERSION).tar.gz
	tla inventory -sB | tar -cf - --no-recursion -T- | \
	  (mkdir -p ../muse-$(VERSION); cd ../muse-$(VERSION) && \
	  tar xf -)
	(cd .. && tar czf muse-$(VERSION).tar.gz muse-$(VERSION) ; \
	  zip -r muse-$(VERSION).zip muse-$(VERSION))

upload: dist
	(cd .. && gpg --detach muse-$(VERSION).tar.gz && \
	  gpg --detach muse-$(VERSION).zip && \
	  scp muse-$(VERSION).zip* muse-$(VERSION).tar.gz* \
	    mwolson@download.gna.org:/upload/muse-el)
