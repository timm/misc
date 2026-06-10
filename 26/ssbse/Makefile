# Marp deck. Single source of truth: talk.md
MD   := talk.md
MARP := npx --yes @marp-team/marp-cli

.PHONY: html pdf pptx watch clean

# fast; no Chrome needed
html:
	$(MARP) $(MD) -o talk.html

# pdf/pptx need Chrome or Chromium on PATH (or set CHROME_PATH)
pdf:
	$(MARP) $(MD) -o talk.pdf --allow-local-files

pptx:
	$(MARP) $(MD) -o talk.pptx --allow-local-files

# live preview, reloads on save
watch:
	$(MARP) -s .

clean:
	rm -f talk.html talk.pdf talk.pptx
