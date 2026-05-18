# vim: ts=2 sw=2 sts=2 et :
SHELL := /bin/bash
OPEN  := $(shell command -v open 2>/dev/null || command -v xdg-open 2>/dev/null || echo true)
need   = @command -v $(1) >/dev/null || { printf "missing: %s (needed for %s)\n" $(1) $(2); exit 1; }

help: ## show help
	@gawk 'BEGIN {FS = ":.*?##"; \
	         printf "\nUsage:\n  make \033[36m<target>\033[0m [VAR=val ...]\n\ntargets:\n"} \
	       /^[~a-zA-Z0-9_%\.\/-]+[ ]*:.*?##/ { \
	         printf("  \033[36m%-20s\033[0m %s\n", $$1, $$2) | "sort" }' $(MAKEFILE_LIST)

push: ## prompt msg, commit -am, push
	@read -p "Reason? " msg; git commit -am "$$msg"; git push; git status

define BASHRC
set -o vi
__gp(){ local b=$$(git branch --show-current 2>/dev/null); [[ -z $$b ]] && return
  [[ -n $$(git status --porcelain 2>/dev/null) ]] && b="$$b*"; echo " $$b"; }
__pw(){ pwd | awk -F/ '{print $$(NF-1)"/"$$NF}'; }
PS1='\[\e[36m\]$$(__pw)\[\e[33m\]$$(__gp) \[\e[0m\][\!]\$$ '
alias ll='ls -la' gs='git status -s' gd='git diff' gl='git log --oneline -20'
endef
export BASHRC

sh: ## launch tuned bash — no rc file
	@bash --rcfile <(echo "$$BASHRC") -i

Font   ?= 5
Cols   ?= 3
Orient ?= landscape ###

check: ## luacheck all .lua files
	$(call need,luacheck,check)
	@luacheck *.lua

doctor: ## check required tools (✓ found, ✗ missing)
	@for e in \
	   "bash|sh target, pdf trap (process subst)" \
	   "lua|run tree.lua" \
	   "gawk|help target (self-doc)" \
	   "git|push target, sh prompt" \
	   "a2ps|pdf target (text → postscript)" \
	   "ps2pdf|pdf target (postscript → pdf, ghostscript)" \
	   "luacheck|check target (lua linter)" \
	   "nvim|vi target (tuned editor)"; do \
	   c=$${e%%|*}; use=$${e##*|}; \
	   if command -v $$c >/dev/null; then \
	     printf "  \033[32m✓\033[0m %-10s used by: %s\n" "$$c" "$$use"; \
	   else \
	     printf "  \033[31m✗\033[0m %-10s missing — can't: %s\n" "$$c" "$$use"; fi; done
	@printf "\nmacOS: brew install lua gawk a2ps ghostscript neovim luarocks && luarocks install luacheck\n"
	@printf   "linux: apt install lua5.4 gawk a2ps ghostscript neovim luarocks && luarocks install luacheck\n"

define VIMRC
set nu rnu cursorline mouse=a termguicolors et ts=2 sw=2 sts=2 ai si
set ignorecase smartcase hlsearch incsearch scrolloff=8 signcolumn=yes
set splitbelow splitright wrap linebreak wildmenu clipboard=unnamedplus
syntax on | filetype plugin indent on
endef
export VIMRC

F ?= $(firstword $(wildcard *.lua))
vi: ## launch tuned nvim — var: F
	$(call need,nvim,vi)
	@nvim --clean -c "$$VIMRC" $F

define LUASSH
style Lua is
alphabets are "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_0"
case sensitive
keywords in Keyword are
  and,false,ipairs,nil,not,or,pairs,rawequal,rawget,rawlen,rawset,
  select,tonumber,tostring,true,type
end keywords
keywords in Keyword_strong are
  assert,break,collectgarbage,do,dofile,else,elseif,"end",error,for,
  function,getmetatable,goto,if,"in",local,load,loadfile,next,pcall,
  print,repeat,require,return,setmetatable,then,until,while,xpcall,
  _G,_VERSION
end keywords
keywords in Label_strong are "^" function end keywords
keywords in Comment are self end keywords
sequences are "[[" Comment "]]","--" Comment,C-string end sequences
end style
endef
export LUASSH

~/tmp/%.pdf : %.lua Makefile ## lua -> pdf via a2ps (vars: Font Cols Orient)
	$(call need,a2ps,pdf)
	$(call need,ps2pdf,pdf)
	@mkdir -p ~/tmp
	@echo "pdfing : $@ ..."
	@D=$$(mktemp -d); trap "rm -rf $$D" EXIT; \
	 mkdir -p $$D/.a2ps; echo "$$LUASSH" > $$D/.a2ps/lua.ssh; \
	 HOME=$$D a2ps -Bj --$(Orient) --line-numbers=1 --highlight-level=normal \
	      --borders=no --pro=color --right-footer="" --left-footer="" \
	      --pretty-print=lua --footer="page %p." -M letter \
	      --font-size=$(Font) --columns $(Cols) \
	      -o - $< | ps2pdf - $@
	@$(OPEN) $@
