.SILENT:

../src/%.lua : ../%.md
	mkdir -p ../src
	gawk -f ../etc/md2lua.awk $^ > $@
	luac -p $@

test: 
	lua $(it).lua
