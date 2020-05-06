F=lua2md

PANDOCFLAGS =                        \
  -s \
  --table-of-contents                \
  --from=markdown                    \
  --indented-code-classes=lua        \
  --highlight-style=pygments         \
  --template=templates/default.html5 \
  --metadata title="$F"              \
  --css=../etc/css/style.css 

all : ok o/$F.html

o/%.html: %.md  Makefile 
	pandoc $< -o $@ $(PANDOCFLAGS)

ok: clean dirs

dirs:; mkdir ./o

clean:; rm -rf ./o

open: o/$F.html
	open o/$F.html
