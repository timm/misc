~/tmp/%.html : %.lua
	pycco -d ~/tmp $<
	echo "p {text-align: right;}"       >> ~/tmp/pycco.css
	echo "code,pre {font-size: small;}" >> ~/tmp/pycco.css
	open $@
