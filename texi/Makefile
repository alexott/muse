.PHONY: all info-only doc clean realclean distclean fullclean install
.PRECIOUS: %.info %.html

include ../Makefile.defs

all: doc

%.info: %.texi
	makeinfo $<

%.html: %.texi
	makeinfo --html --no-split $<

info-only: $(MANUAL).info

doc: $(MANUAL).info $(MANUAL).html

clean: ;

distclean realclean fullclean: clean
	-rm -f $(MANUAL).info $(MANUAL).html

install: $(MANUAL).info
	[ -d $(INFODIR) ] || install -d $(INFODIR)
	install -m 0644 $(MANUAL).info $(INFODIR)/$(MANUAL)
	$(INSTALLINFO) $(INFODIR)/$(MANUAL)
