# vim: ts=2 sw=2 sts=2 et :
SHELL := /bin/bash

help: ## show help
	@gawk 'BEGIN {FS = ":.*?##"; \
	         printf "\nUsage:\n  make \033[36m<target>\033[0m [VAR=val ...]\n\ntargets:\n"} \
	       /^[a-zA-Z0-9_%\.\/-]+:.*?##/ { \
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
	@luacheck *.lua

define VIMRC
set nu rnu cursorline mouse=a termguicolors et ts=2 sw=2 sts=2 ai si
set ignorecase smartcase hlsearch incsearch scrolloff=8 signcolumn=yes
set splitbelow splitright wrap linebreak wildmenu clipboard=unnamedplus
syntax on | filetype plugin indent on
endef
export VIMRC

F ?= $(firstword $(wildcard *.lua))
vi: ## launch tuned nvim — var: F
	@nvim --clean -c "$$VIMRC" $F

~/tmp/%.pdf : %.lua lua.ssh Makefile ## lua -> pdf via a2ps (vars: Font Cols Orient)
	@mkdir -p ~/tmp
	@echo "pdfing : $@ ..."
	@a2ps -Bj --$(Orient) --line-numbers=1 --highlight-level=normal \
	      --borders=no --pro=color --right-footer="" --left-footer="" \
	      --pretty-print=./lua.ssh --footer="page %p." -M letter \
	      --font-size=$(Font) --columns $(Cols) \
	      -o - $< | ps2pdf - $@
	@open $@
