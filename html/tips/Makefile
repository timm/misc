MD_FILES=$(wildcard *.md)
HTML_FILES=$(MD_FILES:.md=.html)

all: check-marked $(HTML_FILES)

check-marked:
	@if ! command -v marked >/dev/null 2>&1; then \
		echo "marked not found. Installing..."; \
		npm install -g marked; \
	fi

%.html: %.md
	cat skin/header.html > $@
	marked $< >> $@
	cat skin/footer.html >> $@

clean:
	rm -f $(HTML_FILES)