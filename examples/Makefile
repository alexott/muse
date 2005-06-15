.PHONY: all examples clean realclean distclean fullclean
EMACS    = emacs
EXAMPLES = README.html README.pdf README.info

all: examples

examples: $(EXAMPLES)

%.html: %
	../scripts/publish html $<

%.pdf: %
	../scripts/publish pdf $<

%.info: %
	../scripts/publish info $<

clean:
	-rm -f missfont.log

realclean distclean fullclean: clean
	-rm -f README.* 
