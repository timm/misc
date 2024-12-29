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
	echo -e "$(HIGH)"; cat ../etc/frog.txt; echo -e "$(SOFT)"

vim: ## vim install
	mkdir -p ~/.vim
	if [[ ! -d ~/.vim/bundle ]]; \
	then git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim; \
	fi
	vim +'PluginInstall --sync' +qa

sh: ## run a shell
	bash --init-file  dotshellrc -i

push: ## commit to main
	- echo -en "$(LOUD)Why this push? $(SOFT)" ;  read x ; git commit -am "$$x" ;  git push
	- git status

word: ## commit to main
	- echo -en "$(LOUD)phrase? $(SOFT)" ;  read x ; figlet -W -f mini $$x | gawk '{print "-- " $$0}' |pbcopy

~/tmp/%.html : %.awk
	pycco -d ~/tmp $^
	echo "p { text-align: right; }" >> ~/tmp/pycco.css
	open $@

~/tmp/all.pdf : *.lua  
	@mkdir -p docs
	@echo "pdfing : $@ ... "
	@a2ps -Bj -A virtual --landscape                           \
		--chars-per-line=80 \
		--line-numbers=1                    \
		--highlight-level=normal  \
		--columns 3                 \
		--borders=no --pro=color \
		--right-footer="" --left-footer=""    \
			--pretty-print="lua.ssh"     \
		--footer="page %p."                     \
		-M letter -o $@.ps $^
	@ps2pdf $@.ps $@; rm $@.ps
