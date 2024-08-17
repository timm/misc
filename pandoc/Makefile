F=lua2md

PANDOCFLAGS =                     \
  -s                               \
  --mathjax                         \
  --from=markdown                    \
  --table-of-contents                 \
  --metadata title="$F"                \
  --css=../etc/css/a.css                \
  --include-after=div.md                 \
  --include-before=menubar.md             \
  --indented-code-classes=lua,numberLines  \
  --css='https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.13.0/css/all.min.css'    

all 			: ok o/$F.html
o/%.html	: %.md  Makefile ; pandoc $< -o $@ $(PANDOCFLAGS)
ok				: clean dirs 
dirs			:						 		 ; mkdir ./o
clean			:								 ; rm -rf ./o
open			: o/$F.html			 ; open o/$F.html
