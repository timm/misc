.SILENT:
all: docs/defmacros.html

FMT= -x toc -x codehilite -x tables -x fenced_code -x footnotes -x attr_list -x admonition
docs/%.html : tmp/%.md
	echo "# $@ ..."
	(bash etc/head.sh $^; python3 -m markdown $(FMT) $^) > $@

tmp/%.md : %.lisp
	echo "# $@ ..."
	gawk -f etc/lisp2md.awk $^ > $@

put:
	git commit -am saving; git push; git status
