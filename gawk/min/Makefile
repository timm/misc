MAKEFLAGS += --silent
SHELL:= /bin/bash#
R=$(shell git rev-parse --show-toplevel)

help : Makefile
	echo ""; printf "usage: make [OPTIONS]\n\n"
	@awk 'BEGIN {FS="[ \t]*:.*##[ \t]*"}  \
	  NF==2 { printf \
           "  \033[36m%-25s\033[0m %s\n","make " $$1,$$2}'  $< \
	| grep -v awk

docs/index.html: docs/shortr.html ##  commit to main
	cp docs/shortr.html docs/index.html

ready:  ##  commit to main
	git add *;git commit -am save;git push;git status

docs/%.html: %.lua ## make html
	(awk '\
     BEGIN            { FS="(-|[ \t]*)?->[ \t]*"}\
     NF==3 && /^-->/ { $$2=gensub(/([A-Za-z0-9_]+):/," `\\1`:  ","g",$$2);\
                       print "--**"$$2"** <br> "$$3 ; next}\
      1' $^ ) > tmp.lua
	echo "docco: $< -> $@"
	docco -l classic  tmp.lua > /dev/null
	awk 'sub(/>tmp.lua</,">$<<") 1 ' docs/tmp.html > $@
	rm tmp.lua 
	cp $R/etc/docco.css docs/docco.css

~/tmp/%.pdf : %.lua  ## make pdf
	@mkdir -p docs
	@echo "pdf-ing $@ ... "
	@a2ps -Bjr    -q                        \
		-L 125 \
		--line-numbers=1                    \
		--highlight-level=normal  \
		--borders=no --pro=color --columns 3 \
		--right-footer="" --left-footer=""    \
		--pretty-print=lua.ssh             \
		--footer="page %p."                     \
		-M letter -o $@.ps $<
	@ps2pdf $@.ps $@; rm $@.ps

        #-H $(Etc)/favicon.html         \
        #--include-before=$(Etc)/head.md \
        #--include-after=$(Etc)/foot.md   \
        #--css style.css \

Pandoc =      \
        -s           \
        -f markdown   \
        --mathjax      \
        --highlight-style kate \
				--css my.css \
  --indented-code-classes=lua

~/tmp/%.html : %.lua 
	echo "$@ ... "
	cp my.css ~/tmp
	sed -e '1d' -e 's/--\[\[//' -e 's/--\]\]//' $< | gawk -f lua2md.awk | pandoc  -o $@ $(Pandoc)
