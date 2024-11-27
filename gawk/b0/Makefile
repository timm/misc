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

~/tmp/%.html : %.lua 
	pycco -d ~/tmp $<
	# cp banner.png ~/tmp
	gawk '/<h1>/ {print "<div class=docs>"; \
		            while(getline < "head.html") {print} ; \
				        print( "<h1>'$<'</h1><hr></div>"); \
					      next} \
			1' $@ > tmp.tmp
	mv tmp.tmp $@

# ~/tmp/%.html : %.auk Makefile head.html
# 	pycco -l bash -d ~/tmp $<
# 	cp banner.png ~/tmp
# 	gawk '/<h1>/ {print "<div class=docs>"; \
# 		            while(getline < "head.html") {print} ; \
# 				        print( "<h1>'$<'</h1><hr></div>"); \
# 					      next} \
# 			1' $@ > tmp.tmp
# 	mv tmp.tmp $@

push: ## commit to main
	- echo -en "$(LOUD)Why this push? $(SOFT)" ;  read x ; git commit -am "$$x" ;  git push
	- git status
 
sh:
	bash --init-file dotbashrc -i

~/tmp/%.pdf : %.lua  ## make pdf
	@mkdir -p docs
	@echo "pdf-ing $@ ... "
	@a2ps -Bjr    -q               \
		--chars-per-line=90          \
		--line-numbers=1               \
		--highlight-level=normal        \
		--columns 3                      \
		--borders=no --pro=color          \
		--right-footer="" --left-footer="" \
		--pretty-print=lua.ssh       \
		--footer="page %p."                  \
		-M letter -o $@.ps $<
	@ps2pdf $@.ps $@; rm $@.ps
	open $@

eg30: ../../../moot/optimize/[chmp]*/*.csv
	$(foreach f,$^, (lua nb.lua --stop 30 --guess $f &);)

eg20: ../../../moot/optimize/[chmp]*/*.csv
	$(foreach f,$^, (lua nb.lua --stop 20 --guess $f &);)

eg60: ../../../moot/optimize/[chmp]*/*.csv
	$(foreach f,$^, (lua nb.lua --stop 60 --guess $f &);)

