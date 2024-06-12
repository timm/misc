SHELL     := bash 
MAKEFLAGS += --warn-undefined-variables

bye:
	echo -n "Why> ";read x; git commit -am "$$x"; git push; git status


src/%.lua : %.md
	mkdir -p docs
	gawk -f etc/md2lua.awk $^ > $@
	luac -p $@

~/tmp/%.html : src/%.lua
	pycco  -d ~/tmp $^ 
	cp marsQueen.png ~/tmp
	open $@

