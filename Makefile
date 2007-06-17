.PHONY: all lisp contrib autoloads examples experimental doc info-only
.PHONY: clean realclean distclean fullclean install-info install-bin install
.PHONY: test dist release debbuild debrevision debrelease upload elpa

include Makefile.defs

SUBDIRS = lisp contrib examples experimental texi

all: autoloads lisp contrib info-only

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

info-only:
	(cd texi && $(MAKE) info-only)

doc texi:
	(cd texi && $(MAKE))

clean:
	for i in $(SUBDIRS); do \
	 (cd $$i && $(MAKE) clean); done

realclean fullclean: clean
	for i in $(SUBDIRS); do \
	 (cd $$i && $(MAKE) realclean); done

install-info:
	(cd texi && $(MAKE) install)

install-bin: autoloads lisp contrib
	(cd lisp && $(MAKE) install)
	(cd contrib && $(MAKE) install)
	(cd experimental && $(MAKE) install-uncompiled)

install: install-bin install-info

test: 
	(cd lisp && $(MAKE) test)

distclean:
	for i in $(SUBDIRS); do \
	 (cd $$i && $(MAKE) distclean); done
	-rm -fr ../$(PROJECT)-$(VERSION)

dist: autoloads distclean
	tla inventory -sB | tar -cf - --no-recursion -T- | \
	  (mkdir -p ../$(PROJECT)-$(VERSION); cd ../$(PROJECT)-$(VERSION) && \
	  tar xf -)
	cp lisp/$(PROJECT)-autoloads.el ../$(PROJECT)-$(VERSION)/lisp

release: dist
	(cd .. && tar -czf $(PROJECT)-$(VERSION).tar.gz \
	    $(PROJECT)-$(VERSION) ; \
	  zip -r $(PROJECT)-$(VERSION).zip $(PROJECT)-$(VERSION) && \
	  gpg --detach $(PROJECT)-$(VERSION).tar.gz && \
	  gpg --detach $(PROJECT)-$(VERSION).zip)

debclean:
	-rm -f ../../dist/$(DISTRIBUTOR)/$(DEBNAME)_*
	-rm -fr ../$(DEBNAME)_$(VERSION)*

debprepare:
	-rm -rf ../$(DEBNAME)-$(VERSION)
	(cd .. && tar -xzf $(PROJECT)-$(VERSION).tar.gz)
	mv ../$(PROJECT)-$(VERSION) ../$(DEBNAME)-$(VERSION)
	(cd .. && tar -czf $(DEBNAME)_$(VERSION).orig.tar.gz \
	    $(DEBNAME)-$(VERSION))
	(cd debian && tla inventory -sB | tar -cf - --no-recursion -T- | \
	  (mkdir -p ../../$(DEBNAME)-$(VERSION)/debian; \
	    cd ../../$(DEBNAME)-$(VERSION)/debian && \
	    tar xf -))

debbuild:
	(cd ../$(DEBNAME)-$(VERSION) && \
	  dpkg-buildpackage -v$(LASTUPLOAD) $(BUILDOPTS) \
	    -us -uc -rfakeroot && \
	  echo "Running lintian ..." && \
	  lintian -i ../$(DEBNAME)_$(VERSION)*.deb || : && \
	  echo "Done running lintian." && \
	  echo "Running linda ..." && \
	  linda -i ../$(DEBNAME)_$(VERSION)*.deb || : && \
	  echo "Done running linda." && \
	  debsign)

debinstall:
	cp ../$(DEBNAME)_$(VERSION)* ../../dist/$(DISTRIBUTOR)

deb: debclean debprepare debbuild debinstall

upload: release
	(cd .. && \
	  scp $(PROJECT)-$(VERSION).zip* $(PROJECT)-$(VERSION).tar.gz* \
	    mwolson@download.gna.org:/upload/muse-el)

elpa: realclean info-only
	rm -fR $(ELPADIR)/$(PROJECT)-$(VERSION)
	rm -f $(ELPADIR)/$(PROJECT)-$(VERSION).tar
	mkdir -p $(ELPADIR)/$(PROJECT)-$(VERSION)
	cp lisp/*.el $(ELPADIR)/$(PROJECT)-$(VERSION)
	cp contrib/*.el $(ELPADIR)/$(PROJECT)-$(VERSION)
	echo '(define-package "$(PROJECT)" "$(VERSION)"' > \
	  $(ELPADIR)/$(PROJECT)-$(VERSION)/$(PROJECT)-pkg.el
	echo '  "$(ELPADESC)")' >> \
	  $(ELPADIR)/$(PROJECT)-$(VERSION)/$(PROJECT)-pkg.el
	cp texi/$(MANUAL).info $(ELPADIR)/$(PROJECT)-$(VERSION)
	echo '* Menu:' > $(ELPADIR)/$(PROJECT)-$(VERSION)/dir
	echo >> $(ELPADIR)/$(PROJECT)-$(VERSION)/dir
	install-info --section "Emacs" "Emacs" \
	  --info-dir=$(ELPADIR)/$(PROJECT)-$(VERSION) \
	  $(ELPADIR)/$(PROJECT)-$(VERSION)/$(MANUAL).info
	rm -f $(ELPADIR)/$(PROJECT)-$(VERSION)/dir.old
	(cd $(ELPADIR) && tar cf $(PROJECT)-$(VERSION).tar \
	  $(PROJECT)-$(VERSION))
