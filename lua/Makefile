F=slope.py

tidy:
	 autopep8 --in-place --aggressive --aggressive $F

publish:
	pycco -d ../docs $F
	git add ../docs/*
	git commit -am new
	git push

# ~/tmp/%.html : %.py
# 	cp bingo.png ~/tmp
# 	gawk '/^.[#]+/ {gsub("-", ""); print $$0"\n\n\n"; next} 1' $^  > ~/tmp/$^
# 	cd ~/tmp; docco -o . $^
# 	echo "p { text-align: right;  }" >> ~/tmp/docco.css
# 	echo "h2 { border-top: 1px solid #CCC;  }" >> ~/tmp/docco.css
# 	open $@

# changes to 3 cols and 101 chars/line
~/tmp/%.pdf: %.lua  ## .lua ==> .pdf
	mkdir -p ~/tmp
	echo "pdf-ing $@ ... "
	a2ps                 \
		-r                 \
		--chars-per-line 80 \
		--file-align=fill      \
		--line-numbers=1        \
		--borders=no             \
		--pro=color               \
		--portrait   \
		--left-title=""            \
		--pretty-print="lua.ssh" \
		--columns  2                 \
		-M letter                     \
		--footer=""                    \
		--right-footer=""               \
	  -o - $< | ps2pdf - $@
	open $@

~/tmp/%.pdf: %.py  ## .py ==> .pdf
	mkdir -p ~/tmp
	echo "pdf-ing $@ ... "
	a2ps               \
		-Br               \
		--quiet            \
		--portrait          \
		--chars-per-line=80  \
		--line-numbers=1      \
		--borders=no           \
		--pro=color             \
		--columns=2              \
		-M letter                 \
		-o - $< | ps2pdf - $@
	open $@


~/tmp/%.md : %.py
	gawk -f py2html.awk $^ > $@

~/tmp/%.html : %.lua
	sed 's/----[-]*/\n--   \n--\n/' $< > ~/tmp/$<
	cd ~/tmp; 	docco -o . $<
	echo 'p { text-align: right;}' >> ~/tmp/docco.css
	echo 'h2 { border-top: 1px solid #CCC; }' >> ~/tmp/docco.css
	echo 'h3 { text-transform: capitalize; }' >> ~/tmp/docco.css

~/tmp/%.html : ~/tmp/%.md
	cp ezr.css ~/tmp
	pandoc -s  -f markdown --number-sections --toc --toc-depth=5 \
         --mathjax \
         --highlight-style monochrome  --css ezr.css \
         --metadata title="$<" \
         -o $@  $<
