.SILENT:
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

%.html : %.md
	(cat head.html; python3 -m markdown -x toc -x codehilite -x tables -x fenced_code -x footnotes $^ ; cat foot.html) > $@
