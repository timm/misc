.SILENT:
FMT= -x toc -x codehilite -x tables -x fenced_code -x footnotes -x attr_list -x admonition
all: aa.html

put:
	git commit -am saving; git push; git status

toc:
	markdown-toc -i aa.md

%.html : %.lisp
	gawk -f lisp2doc.awk $^  | asciidoctor  -  > $@
