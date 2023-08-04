.SILENT:
FMT= -x toc -x codehilite -x tables -x fenced_code -x footnotes -x attr_list -x admonition
all: docs/defmacros.html

put:
	git commit -am saving; git push; git status

tmp/%.md : %.lisp
	echo "# $@ ..."
	gawk -f etc/lisp2md.awk $^ > $@

docs/%.html : tmp/%.md
	echo "# $@ ..."
	(bash etc/head.sh $^; python3 -m markdown $(FMT) $^) > $@
