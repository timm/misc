# vim: ts=2 sw=2 sts=2 et :
SHELL := /bin/bash

OPEN := $(shell command -v open 2>/dev/null \
                || command -v xdg-open 2>/dev/null \
                || echo true)

need = @command -v $(1) >/dev/null || { \
         printf "missing: %s (needed for %s)\n" $(1) $(2); \
         exit 1; }

## help -------------------------------------------------------

help: ## show help
	@gawk 'BEGIN { \
	         FS=":.*?##"; \
	         printf "\nUsage:\n  make "; \
	         printf "\033[36m<target>\033[0m"; \
	         printf " [VAR=val ...]\n\ntargets:\n" } \
	       /^[~a-zA-Z0-9_%\.\/ -]+:.*?##/ { \
	         printf("  \033[36m%-20s\033[0m %s\n", \
	                $$1, $$2) | "sort" }' \
	  $(MAKEFILE_LIST)
	@printf "\ndefaults:\n"
	@gawk 'BEGIN { \
	         p = "^([A-Za-z][A-Za-z0-9]*)[ \t]*\\?="; \
	         p = p "[ \t]*([^#]*[^# \t])[ \t]*#[ \t]*(.+)" } \
	       match($$0, p, a) { \
	         printf("  \033[36m%-8s\033[0m = %-30s %s\n", \
	                a[1], a[2], a[3]) | "sort" }' \
	  $(MAKEFILE_LIST)

## doctor -----------------------------------------------------

doctor: ## check required tools
	@for e in \
	   "lua|run lua files" \
	   "python3|run py files" \
	   "gawk|help target (self-doc)" \
	   "git|version control" \
	   "luacheck|check target (lua linter)" \
	   "ruff|check target (py linter)"; do \
	   c=$${e%%|*}; use=$${e##*|}; \
	   if command -v $$c >/dev/null; then \
	     printf "  \033[32mok\033[0m %-10s used by: %s\n" \
	            "$$c" "$$use"; \
	   else \
	     printf "  \033[31mXX\033[0m %-10s missing: %s\n" \
	            "$$c" "$$use"; fi; done
	@printf "\nmacOS: brew install lua gawk luarocks ruff\n"
	@printf "linux: apt install lua5.4 gawk luarocks ruff\n"
	@printf "then : luarocks install luacheck\n"

## check ------------------------------------------------------

Ignore ?= 211 212 213 311 312 421 422 431 432 611 612 613 614 631 # for check

check: ## lint lua + py
	$(call need,luacheck,check)
	@luacheck --ignore $(Ignore) -- *.lua
	@command -v ruff >/dev/null && ruff check *.py || true

## push -------------------------------------------------------

push: ## add+commit+push+status; msg from cli: make push my note
	@git add -A
	@git commit -m "$(filter-out $@,$(MAKECMDGOALS))" || true
	@git push
	@git status
%:            # swallow the message words so make won't error
	@:

## bench ------------------------------------------------------

Moot ?= ~/gits/moot/optimize # for bench
P    ?= 20                   # for bench

bench: ## run fft over all CSVs under $(Moot), $(P)-way parallel
	$(call need,lua,bench)
	@printf "%4s %4s %4s %4s %4s  %4s %4s %6s  %7s  %s\n" \
	  p10 p30 p50 p70 p90 \#x \#y \#row time dataset
	@find $(Moot) -name "*.csv" | \
	 xargs -P $(P) -I {} lua bench.lua {} | sort -k 11

## hist -------------------------------------------------------

hist: ## paragraph line counts + cum%% (.lua + .py, skip comments)
	@cat $(wildcard *.lua *.py) | grep -Ev '^[[:space:]]*(--|#)' | \
	  gawk 'BEGIN{RS="";FS="\n"} \
	        {d[NF]++; tot++} \
	        END{n=asorti(d,k,"@ind_num_asc"); all=0; \
	            for(i=1;i<=n;i++){ \
	              all+=d[k[i]]; s=""; \
	              for(j=0;j<d[k[i]];j++) s=s"#"; \
	              printf "%3d %3d  %s\n", \
	                k[i], int(all*100/tot), s}}'

## sh ---------------------------------------------------------

sh: ## tuned bash + vi alias (wiped on exit)
	$(call need,nvim,sh)
	$(call need,git,sh)
	@bash --rcfile <(echo "$$BASHRC") -i

define BASHRC
set -o vi
[ -f ~/.bashrc ] && source ~/.bashrc
alias l="lua fft.lua" b="make bench" c="make check"
alias ll='ls -la' gs='git status -s' \
      gd='git diff' gl='git log --oneline -20'

# vi via vim.pack; NVIM_APPNAME isolates pkg data
D=$$(mktemp -d); trap 'rm -rf "$$D"' EXIT
printf '%s\n' "$$INITLUA" > "$$D/init.lua"
alias vi='NVIM_APPNAME=fft nvim --clean -u "$$D/init.lua"'
endef
export BASHRC

## vi ---------------------------------------------------------

F ?= $(firstword $(wildcard *.lua)) # for vi

vi: ## tuned nvim + catppuccin via vim.pack (wiped on exit)
	$(call need,nvim,vi)
	$(call need,git,vi)
	@D=$$(mktemp -d); trap "rm -rf $$D" EXIT; \
	 printf '%s\n' "$$INITLUA" > $$D/init.lua; \
	 NVIM_APPNAME=fft nvim --clean -u $$D/init.lua $F

define INITLUA
vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

vim.o.number = true
vim.o.relativenumber = true
vim.o.cursorline = true
vim.o.mouse = "a"
vim.o.termguicolors = true
vim.o.expandtab = true
vim.o.tabstop = 2
vim.o.shiftwidth = 2
vim.o.softtabstop = 2
vim.o.ignorecase = true
vim.o.smartcase = true
vim.o.scrolloff = 8
vim.o.signcolumn = "yes"
vim.o.splitbelow = true
vim.o.splitright = true
vim.o.wrap = true
vim.o.linebreak = true
vim.o.clipboard = "unnamedplus"
vim.o.fillchars = "vert:│,eob:·"
vim.o.winborder = "rounded"
vim.o.splitkeep = "screen"
vim.o.smoothscroll = true

vim.opt.packpath:prepend(vim.fn.stdpath("data") .. "/site")
vim.pack.add({
  "https://github.com/catppuccin/nvim",
  "https://github.com/nvim-lualine/lualine.nvim",
  "https://github.com/zaldih/themery.nvim",
  "https://github.com/folke/tokyonight.nvim",
  "https://github.com/rebelot/kanagawa.nvim",
  "https://github.com/rose-pine/neovim",
  "https://github.com/EdenEast/nightfox.nvim",
})
vim.cmd.colorscheme("catppuccin-mocha")
vim.cmd("hi! WinSeparator guifg=#7aa2f7 guibg=NONE")
vim.cmd("hi! VertSplit    guifg=#7aa2f7 guibg=NONE")
require("lualine").setup({ options = { theme = "auto" } })
require("themery").setup({
  themes = {
    "catppuccin-mocha","catppuccin-latte",
    "tokyonight-storm","tokyonight-day",
    "kanagawa-wave","kanagawa-dragon",
    "rose-pine","rose-pine-dawn",
    "nightfox","duskfox","carbonfox",
  },
  livePreview = true,
})

vim.api.nvim_create_autocmd("TextYankPost", {
  callback = function() vim.hl.on_yank() end,
})

vim.diagnostic.config({
  virtual_text     = false,
  signs            = true,
  underline        = true,
  update_in_insert = false,
  severity_sort    = true,
  float            = { border = "rounded" },
})

if vim.fn.executable("lua-language-server") == 1 then
  vim.lsp.config("luals", {
    cmd          = {"lua-language-server"},
    filetypes    = {"lua"},
    root_markers = {".luarc.json", ".git"},
    settings = { Lua = {
      diagnostics = { globals = {"vim"} },
      workspace   = { checkThirdParty = false },
      telemetry   = { enable = false },
    }},
  })
  vim.lsp.enable("luals")
  vim.api.nvim_create_autocmd("LspAttach", {
    callback = function(a)
      local c = vim.lsp.get_client_by_id(a.data.client_id)
      if c then c.server_capabilities.semanticTokensProvider = nil end
      local b = { buffer = a.buf, silent = true }
      vim.keymap.set("n", "K",         vim.lsp.buf.hover,         b)
      vim.keymap.set("n", "gd",        vim.lsp.buf.definition,    b)
      vim.keymap.set("n", "gr",        vim.lsp.buf.references,    b)
      vim.keymap.set("n", "<leader>e", vim.diagnostic.open_float, b)
    end,
  })
end
endef
export INITLUA

## pdf --------------------------------------------------------

Font   ?= 5         # for ~/tmp/%.pdf
Cols   ?= 3         # for ~/tmp/%.pdf
Orient ?= landscape # for ~/tmp/%.pdf

~/tmp/%.pdf : %.py Makefile ## py -> pdf via a2ps
	$(call need,a2ps,pdf)
	$(call need,ps2pdf,pdf)
	@mkdir -p ~/tmp
	@echo "pdfing : $@ ..."
	@D=$$(mktemp -d); trap "rm -rf $$D" EXIT; \
	 mkdir -p $$D/.a2ps; \
	 HOME=$$D a2ps -Bj --$(Orient) --line-numbers=1 \
	      --highlight-level=heavy --borders=no \
	      --pro=color --right-footer="" --left-footer="" \
	      --pretty-print=python --footer="page %p." \
	      -M letter --font-size=$(Font) \
	      --columns $(Cols) -o - $< | ps2pdf - $@
	@$(OPEN) $@

~/tmp/%.pdf : %.lua Makefile ## lua -> pdf via a2ps
	$(call need,a2ps,pdf)
	$(call need,ps2pdf,pdf)
	@mkdir -p ~/tmp
	@echo "pdfing : $@ ..."
	@D=$$(mktemp -d); trap "rm -rf $$D" EXIT; \
	 mkdir -p $$D/.a2ps; \
	 echo "$$LUASSH" > $$D/.a2ps/lua.ssh; \
	 awk '/^-- @@/ {if (NR>1) printf "\f"; next} {print}' \
	     $< | \
	 HOME=$$D a2ps -Bj --$(Orient) --line-numbers=1 \
	      --highlight-level=normal --borders=no \
	      --pro=color --right-footer="" --left-footer="" \
	      --pretty-print=lua --footer="page %p." \
	      -M letter --font-size=$(Font) \
	      --columns $(Cols) -o - | ps2pdf - $@
	@$(OPEN) $@

define LUASSH
style Lua is
alphabets are
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_0"
case sensitive

keywords in Keyword are
  and,false,ipairs,nil,not,or,pairs,rawequal,rawget,rawlen,
  rawset,select,tonumber,tostring,true,type
end keywords

keywords in Keyword_strong are
  assert,break,collectgarbage,do,dofile,else,elseif,"end",
  error,for,function,getmetatable,goto,if,"in",local,load,
  loadfile,next,pcall,print,repeat,require,return,
  setmetatable,then,until,while,xpcall,_G,_VERSION
end keywords

keywords in Label_strong are "^" function end keywords
keywords in Comment       are self          end keywords

sequences are
  "[[" Comment "]]","--" Comment,C-string
end sequences
end style
endef
export LUASSH
