.SILENT:
FMT= -x toc -x codehilite -x tables -x fenced_code -x footnotes -x attr_list -x admonition
all: aa.html

put:
	git commit -am saving; git push; git status

toc:
	markdown-toc -i aa.md

# %.html: %.md
# 	echo "# make $@ ..."
# 	pandoc -s  -V css=style0.css  -o $@ $<
# 	gawk -f style.awk $@ > tmp
# 	mv tmp $@

%.html : %.lisp
	gawk -f lisp2md.awk $^  | asciidoctor  -  > $@
	#(sh head.sh $^;  asciidoc tmo-m markdown $(FMT) tmp0; cat foot.html) > $@
	#rm tmp0
