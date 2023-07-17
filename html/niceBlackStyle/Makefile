.SILENT:
all: aa.html

put:
	git commit -am saving; git push; git status

toc:
	markdown-toc -i aa.md

%.html: %.md
	echo "# make $@ ..."
	pandoc -s  -V css=style1.css  -o $@ $<
	gawk -f style.awk $@ > tmp
	mv tmp $@
