F=slope.py

tidy:
	 autopep8 --in-place --aggressive --aggressive $F

publish:
	pycco -d ../docs $F
	git add ../docs/*
	git commit -am new
	git push

# changes to 3 cols and 101 chars/line
~/tmp/%.pdf: %.lua  ## .lua ==> .pdf
	mkdir -p ~/tmp
	echo "pdf-ing $@ ... "
	a2ps                 \
		-Br                 \
		--chars-per-line 100  \
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


