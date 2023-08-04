FMT= -x toc -x codehilite -x tables -x fenced_code -x footnotes -x attr_list -x admonition
all: aa.html

put:
	git commit -am saving; git push; git status

tmp/%.md : %.lisp
	gawk -f etc/lisp2md.awk $^ > $@

docs/%.html : tmp/%.md
	(bash etc/head.sh $^; python3 -m markdown $(FMT) $^) > $@
	cat $^
