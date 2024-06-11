SHELL     := bash 
MAKEFLAGS += --warn-undefined-variables
.SILENT:  

bye:
	echo -n "Why> ";read x; git commit -am "$$x"; git push; git status

docs: ~/tmp/rulr.html

docs/%.md : %.lua
	mkdir -p tmp
	gawk -f etc/txt2md.awk $^ > $*

~/tmp/%.html : %.lua
	pycco  -d ~/tmp $^ 
	echo "p {text-align: right;}" >> ~/tmp/pycco.css
	cp marsQueen.png ~/tmp
	open $@

~/tmp/%.html : %.awk
	pycco -l bash -d ~/tmp $^ 
	echo "p {text-align: right;}" >> ~/tmp/pycco.css
	cp marsQueen.png ~/tmp
	open $@
