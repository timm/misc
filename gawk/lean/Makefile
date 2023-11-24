saved:
	git add *; git commit -am saved; git push

docs/%.html : %.lua
	mkdir -p docs
	pycco -d docs  $^
	cat tricks.css >> docs/pycco.css
	open $@
