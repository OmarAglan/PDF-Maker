PREFIX ?= /usr
BINDIR = $(PREFIX)/bin
SHAREDIR = $(PREFIX)/share
MANDIR = $(SHAREDIR)/man
MAN1DIR = $(MANDIR)/man1
VERSION = $(shell awk '/^Version/ {print $$2 ; exit}' mediawiki2latex.cabal)

all: mediawiki2latex docs

mediawiki2latex: Setup.lhs
	runhaskell Setup.lhs configure
	runhaskell Setup.lhs build

docs: mediawiki2latex.1.gz

mediawiki2latex.1.gz: mediawiki2latex.1
	@gzip -c $< > $@

mediawiki2latex.1: mediawiki2latex.1.in
	@sed "s/MEDIAWIKI2LATEXVERSION/$(VERSION)/" $< > $@


install: all
	install -dm755 "$(DESTDIR)$(BINDIR)"
	install -m755 dist/build/mediawiki2latex/mediawiki2latex "$(DESTDIR)$(BINDIR)"
	install -dm755 "$(DESTDIR)$(MAN1DIR)"
	install -m644 mediawiki2latex.1.gz "$(DESTDIR)$(MAN1DIR)"
	install -dm755 "$(DESTDIR)$(SHAREDIR)/mediawiki2latex/latex"
	install -m644 latex/* "$(DESTDIR)$(SHAREDIR)/mediawiki2latex/latex"


clean:
	rm -rf mediawiki2latex Setup Setup.hi Setup.o ./dist/ mediawiki2latex.1.gz mediawiki2latex.1


deps-update:
	cabal update

deps-install:
	cabal install
