EMACS   = emacs
TARGETS = README.html README.pdf README.info
ELC     = $(patsubst %.el,%.elc,$(wildcard *.el))

all: $(ELC)

doc: $(TARGETS)

%.html: %
	./scripts/publish html $<

%.pdf: %
	./scripts/publish pdf $<

%.info: %
	./scripts/publish info $<

muse-build.elc: scripts/muse-build.el
	@echo muse-build.el is not byte-compiled

%.elc: %.el
	@$(EMACS) --no-init-file --no-site-file -batch \
		-l scripts/muse-build.el -L . \
		-f batch-byte-compile $<

clean:
	-rm -f *.elc *~

realclean distclean fullclean: clean
	-rm -f README.* missfont.log

test: fullclean $(ELC)
	emacs -q -batch -L . -l scripts/muse-build.el \
		-f muse-elint-files muse-*.el

dist: clean
	(cd ..; tar cvzf ~/Public/Emacs/muse.tar.gz muse)

######################################################################

# Makefile rules for Arabic transliteration tool

CFLAGS = -g -DSTANDALONE

atranslit: atranslit.cpp
	g++ $(CFLAGS) -o $@ $<
