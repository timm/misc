docs: ~/tmp/rulr.html

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
