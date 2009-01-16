#
# Makefile for cl-lastfm
#


SBCL=`which sbcl-git`


all: lastfm

dist:
	@echo "Create distribution"

doc:
	@echo "Make API documentation"
	$(SBCL) --script misc/lastfm-atdoc.lisp

cover:
	@echo "Code coverage"
	$(SBCL) --dynamic-space-size 5000 --script misc/lastfm-cover.lisp

www: www/index.xsl www/index.css
	@echo "Make website"
	cd www; $(MAKE)

clean:
	find . \( -name "*.fasl" -o -name "lift.dribble" \) -print -exec rm -fr {} \;
	find www -name "index.html" -print -exec rm -fr {} \;

lastfm: doc cover www