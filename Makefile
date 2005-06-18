.PHONY: all examples doc clean realclean distclean fullclean test dist
EMACS    = emacs
ELC      = $(patsubst %.el,%.elc,$(wildcard *.el))

all: $(ELC)

examples:
	(cd examples && $(MAKE))

doc:
	makeinfo muse.texi
	makeinfo --html --no-split muse.texi

muse-build.elc: scripts/muse-build.el
	@echo muse-build.el is not byte-compiled

%.elc: %.el
	@$(EMACS) -q --no-site-file -batch \
		-l scripts/muse-build.el -L . \
		-f batch-byte-compile $<

clean:
	-rm -f *.elc *~
	(cd examples && $(MAKE) clean)

realclean distclean fullclean: clean
	-rm -f muse.info muse.html
	(cd examples && $(MAKE) distclean)

test: fullclean $(ELC)
	emacs -q -batch -L . -l scripts/muse-build.el \
		-f muse-elint-files muse-*.el

dist: distclean
	(cd ..; tar cvzf ~/Public/Emacs/muse.tar.gz muse)
