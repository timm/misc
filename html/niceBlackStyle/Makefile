.SILENT:
FMT= -x toc -x codehilite -x tables -x fenced_code -x footnotes -x attr_list -x admonition
all: aa.html

put:
	git commit -am saving; git push; git status

toc:
	markdown-toc -i aa.md

%.adoc : %.lisp
	gawk -f lisp2doc.awk $^ > $@

%.html : %.adoc
	 asciidoctor  $^
