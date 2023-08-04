.SILENT:
all: docs/defmacros.html

FMT= -x toc -x codehilite -x tables -x fenced_code -x footnotes -x attr_list -x admonition
docs/%.html : tmp/%.md
	echo "# $@ ..."
	cp etc/style.css docs/
	echo "">tmp_toc
	python3 -m markdown $(FMT) $^ > tmp_md_all
	gawk '/<div class="toc"/,/<\/div>/ { print $0>>"tmp_toc"; next} 1'  tmp_md_all > tmp_md_less
	(bash etc/head.sh $^ ; cat tmp_md_less; echo "</div></body></html>") > $@


tmp/%.md : %.lisp
	echo "# $@ ..."
	gawk -f etc/lisp2md.awk $^ > $@

put:
	git commit -am saving; git push; git status
