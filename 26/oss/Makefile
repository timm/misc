# Beamer deck. Single source of truth: talk.md
# markdown -> pandoc -> beamer -> tectonic (one pass, no bibtex, no reruns)
MD     := talk.md
PDF    := talk.pdf
PANDOC := pandoc $(MD) -t beamer --slide-level=2 --pdf-engine=tectonic
IMGDIR := img
W      := https://commons.wikimedia.org/wiki/Special:FilePath
IMGS   := $(IMGDIR)/ncsu.png $(IMGDIR)/mauss.jpg $(IMGDIR)/gates.jpg \
          $(IMGDIR)/stallman.jpg $(IMGDIR)/torvalds.jpg $(IMGDIR)/potlatch.png \
          $(IMGDIR)/heartbleed.png $(IMGDIR)/harvest.jpg $(IMGDIR)/llama.jpg \
          $(IMGDIR)/whitehouse.jpg $(IMGDIR)/sandbox.jpg $(IMGDIR)/harness.jpg \
          $(IMGDIR)/kula.jpg $(IMGDIR)/tournament.png $(IMGDIR)/talk2007.png $(IMGDIR)/xkcd2347.png

.PHONY: all fast handout view push clean distclean
FYI := $(HOME)/gits/timm/fyi/26oss.pdf

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
$(IMGDIR)/redhat.png:   ; $(call fetch,Red_Hat_Logo_2019.svg)
$(IMGDIR)/ibm.png:      ; $(call fetch,IBM_logo.svg)
$(IMGDIR)/k8s.png:      ; $(call fetch,Kubernetes_logo.svg)
$(IMGDIR)/aws.png:      ; $(call fetch,Amazon_Web_Services_Logo.svg)
# the corporate gifts (and the rival forced to take one), stacked tall (needs ImageMagick montage)
$(IMGDIR)/potlatch.png: $(IMGDIR)/redhat.png $(IMGDIR)/ibm.png $(IMGDIR)/k8s.png $(IMGDIR)/aws.png
	montage $^ -tile 1x -geometry 500x200+0+30 -background white $@
$(IMGDIR)/heartbleed.png: ; $(call fetch,Heartbleed.svg)
$(IMGDIR)/harvest.jpg:  ; $(call fetch,Combine_harvester_cutting_wheat_field_-_geograph.org.uk_-_520759.jpg)
$(IMGDIR)/llama.jpg:    ; $(call fetch,Lama_glama_4zz.jpg)
$(IMGDIR)/whitehouse.jpg: ; $(call fetch,The_White_House_June_2024.jpg)
$(IMGDIR)/sandbox.jpg:  ; $(call fetch,Sandbox_toys_in_Finland.jpg)
$(IMGDIR)/harness.jpg:  ; $(call fetch,Draft_horse$(comma)_Guédelon$(comma)_France.jpg)
$(IMGDIR)/kula.jpg:     ; $(call fetch,Kula_bracelet.jpg)
$(IMGDIR)/xkcd2347.png: ; mkdir -p $(IMGDIR); curl -sL -A "Mozilla/5.0" https://imgs.xkcd.com/comics/dependency.png -o $@
# local: NC State logo, EZR tournament (from ../ssbse), 2007 title slide (cropped)
$(IMGDIR)/ncsu.png:       ; mkdir -p $(IMGDIR); cp ~/gits/timm/fyi/ncsu_brick.png $@
$(IMGDIR)/tournament.png: ; mkdir -p $(IMGDIR); cp ../ssbse/img/tournament.png $@
$(IMGDIR)/talk2007.png:   ; mkdir -p $(IMGDIR); pdftoppm -r 150 -png -f 1 -l 1 "Open source_ can you ignore it_ _ ODP.pdf" $(IMGDIR)/odp && \
	python3 -c "from PIL import Image; Image.open('$(IMGDIR)/odp-01.png').crop((132,585,1030,1262)).save('$@')" && rm -f $(IMGDIR)/odp-01.png
comma := ,

define fetch
	mkdir -p $(IMGDIR); curl -sL -A "Mozilla/5.0" "$(W)/$(1)?width=700" -o $@
endef

# build then open in Skim
view: $(PDF)
	open -a Skim $(PDF)

# commit everything here with a prompted message, push, then copy pdf to fyi
push: $(PDF)
	@read -p "Reason? " msg; \
	 git add -A . ; git commit -m "$$msg"; git push; git status --short .
	cp $(PDF) $(FYI)

clean:
	rm -f $(PDF) talk-handout.pdf

distclean: clean
	rm -rf $(IMGDIR)
