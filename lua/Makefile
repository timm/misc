F=slope.py

tidy:
	 autopep8 --in-place --aggressive --aggressive $F

publish:
	pycco -d ../docs $F
	git add ../docs/*
	git commit -am new
	git push

~/tmp/%.html : %.py
	gawk '/^.[#]+/ {gsub("-", ""); print $$0"\n\n\n"; next} 1' $^  > ~/tmp/$^
	cd ~/tmp; docco -o . $^
	echo "p { text-align: right;  }" >> ~/tmp/docco.css
	echo "h2 { border-top: 1px solid #CCC;  }" >> ~/tmp/docco.css
	open $@

# changes to 3 cols and 101 chars/line
~/tmp/%.pdf: %.lua  ## .lua ==> .pdf
	mkdir -p ~/tmp
	echo "pdf-ing $@ ... "
	a2ps                 \
		-Br                 \
		--chars-per-page 100 \
		--file-align=fill      \
		--line-numbers=1        \
		--borders=no             \
		--pro=color               \
		--left-title=""            \
		--pretty-print="lua.ssh" \
		--columns  3                 \
		-M letter                     \
		--footer=""                    \
		--right-footer=""               \
	  -o	 $@.ps $<
	ps2pdf $@.ps $@; rm $@.ps    
	open $@

~/tmp/%.pdf: %.py  ## .py ==> .pdf
	mkdir -p ~/tmp
	echo "pdf-ing $@ ... "
	a2ps               \
		-Br               \
		--quiet            \
		--landscape         \
		--chars-per-line=90 \
		--line-numbers=1      \
		--borders=no           \
		--pro=color             \
		--columns=3              \
		-M letter                 \
		-o - $< | ps2pdf - $@
	open $@
