# Beamer deck. Single source of truth: talk.md
# markdown -> pandoc -> beamer -> tectonic (one pass, no bibtex, no reruns)
MD     := talk.md
PDF    := talk.pdf
PANDOC := pandoc $(MD) -t beamer --slide-level=2 --pdf-engine=tectonic
IMGDIR := img
W      := https://commons.wikimedia.org/wiki/Special:FilePath
IMGS   := $(IMGDIR)/ncsu.png $(IMGDIR)/mauss.jpg $(IMGDIR)/gates.jpg $(IMGDIR)/stallman.jpg $(IMGDIR)/torvalds.jpg

.PHONY: all fast handout push clean distclean

# everything: slides + 4-up handout
all: $(PDF) handout

# one pass, slides only
fast: $(PDF)

$(PDF): $(MD) $(IMGS)
	$(PANDOC) -o $@

handout: $(MD) $(IMGS)
	$(PANDOC) -V classoption=handout -o talk-handout.pdf

# images cached once; delete img/ to refetch
$(IMGDIR)/mauss.jpg:    ; $(call fetch,Marcel_Mauss.jpg)
$(IMGDIR)/gates.jpg:    ; $(call fetch,Bill_Gates_Letter_to_Hobbyists.jpg)
$(IMGDIR)/stallman.jpg: ; $(call fetch,Richard_Stallman_-_Fête_de_l'Humanité_2014_-_010.jpg)
$(IMGDIR)/torvalds.jpg: ; $(call fetch,LinuxCon_Europe_Linus_Torvalds_03_(cropped).jpg)

define fetch
	mkdir -p $(IMGDIR); curl -sL -A "Mozilla/5.0" "$(W)/$(1)?width=600" -o $@
endef

# commit everything here with a prompted message, then push
push:
	@read -p "Reason? " msg; \
	 git add -A . ; git commit -m "$$msg"; git push; git status --short .

clean:
	rm -f $(PDF) talk-handout.pdf

distclean: clean
	rm -rf $(IMGDIR)
