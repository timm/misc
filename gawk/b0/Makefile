SHELL:= /bin/bash#
.SILENT: # no output

LOUD = \033[1;34m#
HIGH = \033[1;33m#
SOFT = \033[0m#

help: ## show help
	grep '^[a-z].*:.*##' $(MAKEFILE_LIST) \
	| sort \
	| gawk 'BEGIN {FS="##"; print "\n$(LOUD)make$(SOFT) [OPTIONS]\n"} \
	              {sub(/:.*/,"",$$1); \
                 printf("$(LOUD)%10s$(SOFT) %s\n",$$1,$$2)}'

%.awk : %.auk Makefile
	sed -E 's/\.([^0-9\\*\$$\\+])([a-zA-Z0-9_]*)/["\1\2"]/g' $< > $@

~/tmp/%.html : %.auk Makefile head.html
	pycco -l bash -d ~/tmp $<
	cp banner.png ~/tmp
	gawk '/<h1>/ {print "<div class=docs>"; \
		            while(getline < "head.html") {print} ; \
				        print( "<h1>'$<'</h1><hr></div>"); \
					      next} \
			1' $@ > tmp.tmp
	mv tmp.tmp $@

push: ## commit to main
	- echo -en "$(LOUD)Why this push? $(SOFT)" ;  read x ; git commit -am "$$x" ;  git push
	- git status
 
sh:
	bash --init-file dotbashrc -i
