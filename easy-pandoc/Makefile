CODE ?= kate
STYLE ?= elegant_bootstrap_menu

 %.html : %.md
	@pandoc -s --toc \
		--highlight-style $(CODE) \
		--template=./templates/$(STYLE).html \
		$^ -o $@

styles:
	pandoc --list-highlight-styles

saved:
	@git commit -am saved; git push; git status
