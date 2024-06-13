.SILENT:
bye:
	git commit -am as; git push; git status

../src/%.lua : ../%.md
	mkdir -p ../src
	gawk -f ../etc/md2lua.awk $^ > $@
	luac -p $@

it?=rulr
test: $x.lua 
	echo "% lua $x.lua\n" | tee $x.out
	(cd ..; $(MAKE) src/$(it).lua;)
	LUA_PATH="../src/?.lua;;" lua $(x).lua  | tee -a $x.out
