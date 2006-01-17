.PHONY: all lisp autoloads examples experimental doc clean realclean distclean fullclean install-info install-bin install test dist release debbuild debrevision debrelease upload
.PRECIOUS: %.info %.html

include Makefile.defs

SUBDIRS = lisp examples experimental

all: autoloads lisp muse.info

lisp:
	(cd lisp && $(MAKE))

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

doc: muse.info muse.html

clean:
	for i in $(SUBDIRS); do \
	 (cd $$i && $(MAKE) clean); done

realclean fullclean: clean
	-rm -f muse.info muse.html
	for i in $(SUBDIRS); do \
	 (cd $$i && $(MAKE) distclean); done

install-info: muse.info
	[ -d $(INFODIR) ] || install -d $(INFODIR)
	install -m 0644 muse.info $(INFODIR)/muse
	$(INSTALLINFO) $(INFODIR)/muse

install-bin: lisp
	(cd lisp && $(MAKE) install)

install: install-bin install-info

test: 
	(cd lisp && $(MAKE) test)

distclean: realclean
	-rm -fr ../muse-$(VERSION)

dist: distclean
	tla inventory -sB | tar -cf - --no-recursion -T- | \
	  (mkdir -p ../muse-$(VERSION); cd ../muse-$(VERSION) && \
	  tar xf -)
	rm -fr ../muse-$(VERSION)/debian

release: dist
	(cd .. && tar -czf muse-$(VERSION).tar.gz muse-$(VERSION) ; \
	  zip -r muse-$(VERSION).zip muse-$(VERSION) && ;
	  gpg --detach muse-$(VERSION).tar.gz && \
	  gpg --detach muse-$(VERSION).zip)

debbuild:
	(cd ../muse-el-$(VERSION) && \
	  dpkg-buildpackage -v$(LASTUPLOAD) $(BUILDOPTS) \
	    -us -uc -rfakeroot && \
	  echo "Running lintian ..." && \
	  lintian -i ../muse-el_$(VERSION)*.deb || : && \
	  echo "Done running lintian." && \
	  debsign)
	cp ../muse-el_$(VERSION)* ../../dist

debrevision: dist
	-rm -f ../../dist/muse-el_*
	-rm -f ../muse-el_$(VERSION)-*
	-rm -fr ../muse-el-$(VERSION)
	mv ../muse-$(VERSION) ../muse-el-$(VERSION)
	cp -r debian ../muse-el-$(VERSION)
	-rm -fr ../muse-el-$(VERSION)/debian/.arch-ids
	$(MAKE) debbuild

debrelease: dist
	-rm -f ../../dist/muse-el_*
	-rm -f ../muse-el_$(VERSION)*
	-rm -fr ../muse-el-$(VERSION)
	mv ../muse-$(VERSION) ../muse-el-$(VERSION)
	(cd .. && tar -czf muse-el_$(VERSION).orig.tar.gz muse-el-$(VERSION))
	cp -r debian ../muse-el-$(VERSION)
	-rm -fr ../muse-el-$(VERSION)/debian/.arch-ids
	$(MAKE) debbuild

upload: release
	(cd .. && scp muse-$(VERSION).zip* muse-$(VERSION).tar.gz* \
	    mwolson@download.gna.org:/upload/muse-el)
