# ==============================================================================
# tar4 - Minimal Task Runner
# ==============================================================================

SHELL := /bin/bash
GIT_ROOT := $(shell git rev-parse --show-toplevel 2>/dev/null)
ETC := $(GIT_ROOT)/etc

CLS    := '\033[H\033[J'
cRESET := '\033[0m'
cYELLOW:= '\033[1;33m'

help: ## show help
	@gawk -f $(ETC)/help.awk $(MAKEFILE_LIST)

push: ## save to cloud
	@read -p "Reason? " msg; git commit -am "$$msg"; git push; git status

sh: ## demo of my shell
	@-echo -e $(CLS)$(cYELLOW); figlet -W -f slant tar4; echo -e $(cRESET)
	@-bash --init-file $(ETC)/bashrc -i

Font ?= 4.5 # pdf font size
Cols ?= 3   # pdf columns
LPC  ?= 120 # lines per pdf column; packs formfeed sections

~/tmp/%.pdf: %.py $(MAKEFILE_LIST) ## .py ==> .pdf (Font= Cols= LPC=)
	@mkdir -p ~/tmp
	@echo "pdf-ing $@ ... "
	@a2ps -Bj --quiet --landscape --line-numbers=1 \
	   --highlight-level=heavy --borders=no --pro=color \
	   --right-footer="" --left-footer="" \
	   --pretty-print=python --footer="$< :: page %p." \
	   -M letter --center-title="" \
	   --font-size=$(Font) --columns $(Cols) -o - \
	   <(awk -v C=$(LPC) 'BEGIN{RS="\f"; ORS=""} \
	      {n=split($$0,L,"\n")-1; \
	       if($$0==""){printf "\f"; pos=0; next} \
	       if(NR>1 && pos>0 && pos+n>C){printf "\f"; pos=0} \
	       printf "%s",$$0; pos+=n}' $<) \
	 | ps2pdf - $@
	@open $@
