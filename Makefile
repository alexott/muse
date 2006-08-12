.PHONY: all lisp contrib autoloads examples experimental doc clean realclean
.PHONY: distclean fullclean install-info install-bin install test dist release
.PHONY: debbuild debrevision debrelease upload
.PRECIOUS: %.info %.html

include Makefile.defs

SUBDIRS = lisp contrib examples experimental

all: autoloads lisp contrib $(MANUAL).info

lisp:
	(cd lisp && $(MAKE))

contrib:
	(cd contrib && $(MAKE))

autoloads:
	(cd lisp && $(MAKE) autoloads)

examples:
	(cd examples && $(MAKE))

experimental:
	(cd experimental && $(MAKE))

%.info: %.texi
	makeinfo $<

%.html: %.texi
	makeinfo --html --no-split $<

doc: $(MANUAL).info $(MANUAL).html

clean:
	for i in $(SUBDIRS); do \
	 (cd $$i && $(MAKE) clean); done

realclean fullclean: clean
	-rm -f $(MANUAL).info $(MANUAL).html
	for i in $(SUBDIRS); do \
	 (cd $$i && $(MAKE) realclean); done

install-info: $(MANUAL).info
	[ -d $(INFODIR) ] || install -d $(INFODIR)
	install -m 0644 $(MANUAL).info $(INFODIR)/$(MANUAL)
	$(INSTALLINFO) $(INFODIR)/$(MANUAL)

install-bin: autoloads lisp contrib
	(cd lisp && $(MAKE) install)
	(cd contrib && $(MAKE) install)
	(cd experimental && $(MAKE) install-uncompiled)

install: install-bin install-info

test: 
	(cd lisp && $(MAKE) test)

distclean:
	-rm -f $(MANUAL).info $(MANUAL).html
	for i in $(SUBDIRS); do \
	 (cd $$i && $(MAKE) distclean); done
	-rm -fr ../$(PROJECT)-$(VERSION)

dist: autoloads distclean
	tla inventory -sB | tar -cf - --no-recursion -T- | \
	  (mkdir -p ../$(PROJECT)-$(VERSION); cd ../$(PROJECT)-$(VERSION) && \
	  tar xf -)
	cp lisp/$(PROJECT)-autoloads.el ../$(PROJECT)-$(VERSION)/lisp
	rm -fr ../$(PROJECT)-$(VERSION)/debian

release: dist
	(cd .. && tar -czf $(PROJECT)-$(VERSION).tar.gz \
	    $(PROJECT)-$(VERSION) ; \
	  zip -r $(PROJECT)-$(VERSION).zip $(PROJECT)-$(VERSION) && \
	  gpg --detach $(PROJECT)-$(VERSION).tar.gz && \
	  gpg --detach $(PROJECT)-$(VERSION).zip)

debbuild:
	(cd ../$(DEBNAME)-$(VERSION) && \
	  dpkg-buildpackage -v$(LASTUPLOAD) $(BUILDOPTS) \
	    -us -uc -rfakeroot && \
	  echo "Running lintian ..." && \
	  lintian -i ../$(DEBNAME)_$(VERSION)*.deb || : && \
	  echo "Done running lintian." && \
	  debsign)
	cp ../$(DEBNAME)_$(VERSION)* ../../dist

debrevision: dist
	-rm -f ../../dist/$(DEBNAME)_*
	-rm -f ../$(DEBNAME)_$(VERSION)-*
	-rm -fr ../$(DEBNAME)-$(VERSION)
	mv ../$(PROJECT)-$(VERSION) ../$(DEBNAME)-$(VERSION)
	cp -r debian ../$(DEBNAME)-$(VERSION)
	-rm -fr ../$(DEBNAME)-$(VERSION)/debian/.arch-ids
	$(MAKE) debbuild

debrelease: dist
	-rm -f ../../dist/$(DEBNAME)_*
	-rm -f ../$(DEBNAME)_$(VERSION)*
	-rm -fr ../$(DEBNAME)-$(VERSION)
	mv ../$(PROJECT)-$(VERSION) ../$(DEBNAME)-$(VERSION)
	(cd .. && tar -czf $(DEBNAME)_$(VERSION).orig.tar.gz \
	    $(DEBNAME)-$(VERSION))
	cp -r debian ../$(DEBNAME)-$(VERSION)
	-rm -fr ../$(DEBNAME)-$(VERSION)/debian/.arch-ids
	$(MAKE) debbuild

upload: release
	(cd .. && \
	  scp $(PROJECT)-$(VERSION).zip* $(PROJECT)-$(VERSION).tar.gz* \
	    mwolson@download.gna.org:/upload/muse-el)
