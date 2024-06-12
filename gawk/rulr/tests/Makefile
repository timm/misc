../src/%.lua : ../%.md
	mkdir -p ../src
	gawk -f ../etc/md2lua.awk $^ > $@

test: 
	lua $(it).lua
