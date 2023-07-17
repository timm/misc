.SILENT:
all: aa.html

put:
	git commit -am saving; git push; git status

%.html: %.md
	echo "# make $@ ..."
	markdown-toc -i $<
	pandoc -s -V css=style1.css  -o $@ $<
	gawk -f style.awk $@ > tmp
	mv tmp $@
