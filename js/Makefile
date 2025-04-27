DOCS = $(HOME)/tmp
SRC  ?= some.js
TMP  = tmp.js
HEAD ?= header.txt

.PHONY: all docco fix clean

all: $(DOCS) docco fix

$(DOCS):
	mkdir -p $@

docco: $(HEAD) $(SRC)
	( \
	  cat $(HEAD); \
	  echo "// $(notdir $(SRC)): this file"; \
	  cat $(SRC); \
	) > $(DOCS)/$(SRC)
	cd $(DOCS);  docco  $(SRC)

fix:
	gawk '{ \
		gsub(/525px/, "425px"); \
		gsub(/font-size: 12px;/, "font-size: 11px;"); \
		gsub(/line-height: 16px;/, "line-height: 13px;"); \
		print \
	}' $(DOCS)/docco.css > $(DOCS)/docco.css.tmp && mv $(DOCS)/docco.css.tmp $(DOCS)/docco.css

~/tmp/%.pdf : %.js Makefile
		@echo "pdfing : $@ ..."
		@a2ps -Bj --landscape \
			--chars-per-line=90 \
			--line-numbers=1 \
			--highlight-level=normal \
			--columns=3 \
			--borders=no --pro=color \
			--right-footer="" --left-footer="" \
			--pretty-print=./js.ssh\
			--footer="page %p." \
			-M letter -o - $< | ps2pdf - $@
		open $@
	
clean:
	rm -rf $(DOCS)
