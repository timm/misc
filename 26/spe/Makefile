# ------------------------------------------------------------------
# PROMPT (the request this Makefile implements):
#
#   Build a pycco version of ezr.py. Find comments on the same line as
#   "def" and move them to the line above. Above that comment line, put
#   the signature of the call (in backticks). Do not change ./ezr.py;
#   write a temp version docs/ezr.py with the added stuff. Include the
#   right-side "Fork me on GitHub" banner. Justify paragraph text.
#
# Usage:   make docs          # -> docs/ezr.html
#          make clean
# Knobs:   make docs REPO=https://github.com/timm/ezr JUSTIFY=right
# ------------------------------------------------------------------

SRC     = ezr.py
DOCS    = docs
TMP     = $(DOCS)/ezr.py
HTML    = $(DOCS)/ezr.html
REPO    = https://github.com/timm/ezr
JUSTIFY = justify

.PHONY: docs clean

docs: $(HTML)

$(TMP): $(SRC) $(DOCS)/pyccoize.py
	python3 $(DOCS)/pyccoize.py $(SRC) $(TMP)

$(HTML): $(TMP) $(DOCS)/postfix.py
	pycco $(TMP) -d $(DOCS)
	python3 $(DOCS)/postfix.py $(HTML) $(REPO) $(JUSTIFY)

clean:
	rm -f $(TMP) $(HTML) $(DOCS)/pycco.css
