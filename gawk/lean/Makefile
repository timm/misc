saved:
	git add *; git commit -am saved; git push; git status

docs/%.html : %.lua
	mkdir -p docs
	pycco -d docs  $^
	cat tricks.css >> docs/pycco.css
	open $@
