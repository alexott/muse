.PHONY: all examples clean realclean distclean fullclean
.PRECIOUS: %.html %.pdf %.info

include ../Makefile.defs

EXAMPLES = QuickStart.html QuickStart.pdf QuickStart.info

all: examples

examples: $(EXAMPLES)

%.html: %.muse
	EMACS=$(EMACS) SITEFLAG=$(SITEFLAG) ../scripts/publish html $<

%.pdf: %.muse
	EMACS=$(EMACS) SITEFLAG=$(SITEFLAG) ../scripts/publish info-pdf $<

%.info: %.muse
	EMACS=$(EMACS) SITEFLAG=$(SITEFLAG) ../scripts/publish info $<

clean:
	-rm -f missfont.log

realclean distclean fullclean: clean
	-rm -f $(EXAMPLES)
