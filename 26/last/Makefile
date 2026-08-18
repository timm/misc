MD   = $(wildcard *.md)
HTML = $(MD:.md=.html)

all: $(HTML)

%.html: %.md etc/md2html.awk etc/style.css
	awk -f etc/md2html.awk -v css=etc/style.css $< > $@

what1.html: what1.md etc/prep.awk
	awk -f etc/prep.awk $< > $@

clean: ; rm -f $(HTML)
