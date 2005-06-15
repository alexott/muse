.PHONY: all examples clean realclean distclean fullclean test dist
EMACS    = emacs
ELC      = $(patsubst %.el,%.elc,$(wildcard *.el))

all: $(ELC)

examples:
	(cd examples && $(MAKE))

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
	(cd examples && $(MAKE) distclean)

test: fullclean $(ELC)
	emacs -q -batch -L . -l scripts/muse-build.el \
		-f muse-elint-files muse-*.el

dist: clean
	(cd ..; tar cvzf ~/Public/Emacs/muse.tar.gz muse)
