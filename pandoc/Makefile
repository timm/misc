
PANDOCFLAGS =                        \
  --table-of-contents                \
  --pdf-engine=xelatex               \
  --from=markdown                    \
  --number-sections                  \
  --indented-code-classes=lua        \
  --highlight-style=pygments       \
  --include-in-header=header.tex \
  -V mainfont="Palatino"             \
  -V documentclass=report            \
  -V papersize=A5                    \
  -V geometry:margin=1in

  #--template=templates/book.latex \
X=book

all: ok o/$X.pdf o/$X.html

ok: clean dirs

o/%.html: %.md  Makefile 
	pandoc $< -o $@ $(PANDOCFLAGS)

o/%.pdf: %.md  Makefile 
	pandoc $< -o $@ $(PANDOCFLAGS)

o/%.epub: %.md Makefile 
	pandoc $< -o $@ $(PANDOCFLAGS)

dirs:
	mkdir ./o

clean: 
	rm -rf ./o

open: o/$X.pdf
	open o/$X.pdf
