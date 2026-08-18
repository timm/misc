MD   = $(wildcard *.md)
HTML = $(MD:.md=.html)

all: $(HTML)

%.html: %.md etc/md2html.awk etc/style.css
	awk -f etc/md2html.awk -v css=etc/style.css $< > $@

# what1: pandoc does the markdown; sed only paints "(n)" as a round icon
what1.html: what1.md etc/head.html etc/toggle.html
	sed -E 's/^\(([0-9]+)\) /<span class="num">\1<\/span> /' $< | \
	pandoc -s -f markdown-smart -t html5 --reference-location=section -M document-css=false \
	       -H etc/head.html -B etc/toggle.html -o $@

clean: ; rm -f $(HTML)
