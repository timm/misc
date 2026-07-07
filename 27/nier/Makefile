# Build for ICSE'27 NIER paper.
# Uses latexmk (same engine Overleaf runs under the hood: pdflatex + bibtex).
# Overleaf itself ignores Makefiles; just upload nier.tex + nier.bib there
# and it compiles with the identical toolchain.

PAPER ?= nier

.PHONY: all fast clean watch

all: $(PAPER).pdf

# one pdflatex pass, no bibtex refresh; quick check of text edits
fast:
	pdflatex -interaction=nonstopmode -halt-on-error $(PAPER)

%.pdf: %.tex %.bib
	latexmk -pdf -bibtex -interaction=nonstopmode -halt-on-error $*

watch: $(PAPER).tex $(PAPER).bib
	latexmk -pdf -bibtex -pvc $(PAPER)

clean:
	latexmk -C $(PAPER)
	rm -f $(PAPER).bbl $(PAPER).run.xml
