EMACS   = emacs
TARGETS = README.html README.pdf README.info
ELC     = $(patsubst %.el,%.elc,$(wildcard *.el))

all: $(TARGETS) $(ELC)

%.html: %
	./publish html $<

%.pdf: %
	./publish pdf $<

%.info: %
	./publish info $<

muse-build.elc: muse-build.el
	@echo muse-build.el is not byte-compiled

%.elc: %.el
	@$(EMACS) --no-init-file --no-site-file -batch -l muse-build.el -L . \
		-f batch-byte-compile $<

clean:
	-rm -f *.elc *~

realclean distclean fullclean: clean
	-rm -f README.*

test: fullclean $(TARGETS) $(ELC)
	make clean
	emacs -q -batch -L . -l muse-build.el -f muse-elint-files muse-*.el

dist: clean
	(cd ..; tar cvzf ~/Public/Emacs/muse.tar.gz muse)

######################################################################

# Makefile rules for Arabic transliteration tool

CFLAGS = -g -DSTANDALONE

atranslit: atranslit.cpp
	g++ $(CFLAGS) -o $@ $<
