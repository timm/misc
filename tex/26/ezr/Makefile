# Makefile for Beamer Slides with Minted
# Dumps aux files to ~/tmp to keep the working directory clean.

TEXFILE = slides.tex
PDFFILE = slides.pdf
TMP_DIR = ~/tmp

# Phony targets
.PHONY: all clean

all: $(PDFFILE)

$(PDFFILE): $(TEXFILE)
	@echo "--- Compiling $(TEXFILE) (Pass 1) ---"
	pdflatex -shell-escape $(TEXFILE)
	@echo "--- Compiling $(TEXFILE) (Pass 2 for references/layout) ---"
	pdflatex -shell-escape $(TEXFILE)
	@echo "--- Cleaning up auxiliary files to $(TMP_DIR) ---"
	@mkdir -p $(TMP_DIR)
	@mv *.aux *.log *.nav *.out *.snm *.toc *.vrb $(TMP_DIR)/ 2>/dev/null || true
	@if [ -d _minted-slides ]; then \
		rm -rf $(TMP_DIR)/_minted-slides; \
		mv _minted-slides $(TMP_DIR)/; \
	fi
	@echo "--- Done! Generated $(PDFFILE) ---"

clean:
	@echo "--- Removing PDF and tmp files ---"
	rm -f $(PDFFILE)
	rm -rf $(TMP_DIR)/_minted-slides
	rm -f $(TMP_DIR)/slides.aux $(TMP_DIR)/slides.log $(TMP_DIR)/slides.nav 
	rm -f $(TMP_DIR)/slides.out $(TMP_DIR)/slides.snm $(TMP_DIR)/slides.toc $(TMP_DIR)/slides.vrb
