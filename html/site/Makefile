all: $(wildcard docs/*.html)

FMT= -x toc -x codehilite -x tables -x fenced_code -x footnotes -x attr_list -x admonition

define html
        echo "# $@ ..."
	python3 -m markdown $(FMT) $^ > tmp_md
	./etc/page.sh tmp_md  > $@
	rm tmp_md
endef

# to build this : if any of this is new      ; then do this
docs/style.css  : etc/style.css              ; cp $^ $@
docs/%.html     : tmp/%.md docs/style.css    ; $(call html)
docs/%.html     : %.md     docs/style.css    ; $(call html)

tmp/%.md : %.lisp
	echo "# $@ ..."
	gawk -f etc/lisp2md.awk $^ > $@

put:
	git commit -am saving; git push; git status
