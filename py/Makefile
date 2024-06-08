saved:
	git commit -am as; git push; git status

~/tmp/%.html : %.py
	pycco -d ~/tmp $^
	echo 'p {text-align: right;}' >> ~/tmp/pycco.css
	sed -i '' 's/2ez.py : /<img src=2ez.png align=left width=170>&/' $@
	cp 2ez.png ~/tmp
	open $@

~/tmp/%.pdf: %.py  ## .lua ==> .pdf
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
		--columns  3                 \
		-M letter                     \
		--footer=""                    \
		--right-footer=""               \
	  -o	 $@.ps $<
	ps2pdf $@.ps $@; rm $@.ps    
	open $@

