MD   = $(wildcard *.md)
HTML = $(MD:.md=.html)

all: $(HTML)

%.html: %.md etc/md2html.awk etc/style.css
	awk -f etc/md2html.awk -v css=etc/style.css $< > $@

clean: ; rm -f $(HTML)
