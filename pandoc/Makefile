F=lua2md

PANDOCFLAGS =                        \
  -s                                 \
  --mathjax  \
  --table-of-contents                \
  --from=markdown                    \
  --indented-code-classes=lua,numberLines        \
  --highlight-style=tango         \
  --metadata title="$F"              \
  --css=../etc/css/style.css  \
  --css='https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.13.0/css/all.min.css'    \
  --css='https://fonts.googleapis.com/css2?family=Muli&display=swap' \
  --include-before=menubar.md        \
  --include-after=div.md        \
  -V numberLines=true


all : ok o/$F.html

o/%.html: %.md  Makefile 
	pandoc $< -o $@ $(PANDOCFLAGS)

ok: clean dirs

dirs:; mkdir ./o

clean:; rm -rf ./o

open: o/$F.html
	open o/$F.html
