.SILENT:
all: ready $(wildcard docs/*.html)

ready:
	mkdir -p etc tmp test docs

FMT= -x toc -x codehilite -x tables -x fenced_code -x footnotes -x attr_list -x admonition

define html
echo "# $@ ..."
python3 -m markdown $(FMT) $^ > tmp_md
./etc/page.sh tmp_md  > $@
endef

# to build this : if any of this is new      ; then do this
docs/style.css  : etc/style.css              ; cp $^ $@
docs/%.html     : tmp/%.md docs/style.css    ; $(call html)
docs/%.html     : %.md     docs/style.css    ; $(call html)

tmp/%.md : %.lisp
	echo "# $@ ..."
	gawk -f etc/lisp2md.awk $^ > $@

tmp/%.md : %.py
	echo "# $@ ..."
	gawk -f etc/py2md.awk $^ > $@

put:
	git commit -am saving; git push; git status
