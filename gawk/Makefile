# vim: ts=2 sw=2 noet
.SILENT:
SHELL = /bin/bash

help: ## show this help
	echo -e  "\n\033[1;36mmake\033[0m [\033[1;33moptions\033[0m]:\n"
	grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| gawk 'BEGIN {FS = ":.*?## "} \
               	{printf "  \033[1;32m%-10s\033[0m %s\n", $$1, $$2}'

#------------------------------------------------------
CDATA=~/gits/timm/moot/classify/diabetes.csv

ok : *.awk; chmod +x $^

nbc : ok ## run naive bayes classifier
	./bin.awk $(CDATA) | ./nbc.awk | sort -n | uniq -c

confuse : ok ## run classifier with confusion matrix
	{ ./bin.awk $(CDATA) | ./nbc.awk | ./confuse.awk; } 2>/dev/null

# misc ------------------------------------------------
pull: ## update from main
	git pull

push: ## commit to main
	echo -en "\033[1;31mWhy this push?\033[0m " 
	read x ; git commit -am "$$x" ;  git push
	git status


~/tmp/%.pdf : %.lua Makefile lua.ssh
	@echo "pdfing : $@ ... "
	@a2ps -Bj --landscape      \
		--font-size=5             \
		--line-numbers=1           \
		--highlight-level=normal    \
		--columns 2                  \
		--borders=no --pro=color      \
		--right-footer="" --left-footer=""  \
		--pretty-print=lua.ssh               \
		--footer="page %p."                   \
		--portrait \
		-M letter                              \
		-o - $< | ps2pdf - $@
	open $@
