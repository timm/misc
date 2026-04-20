# vim: ts=2 sw=2 sts=2 et  :
I:= $(shell git rev-parse --show-toplevel)

help: ## show help.
	@gawk  '\
		BEGIN {FS = ":.*?##"; \
           printf "\nUsage:\n  make \033[36m<target>\033[0m\n\ntargets:\n"} \
         /^[~a-z0-9A-Z_%\.\/-]+:.*?##/ { \
           printf("  \033[36m%-15s\033[0m %s\n", $$1, $$2) | "sort " } \
		'$(MAKEFILE_LIST)

push: ## save to cloud
	@read -p "Reason? " msg; git commit -am "$$msg"; git push; git status

BLUE  := \033[34m
YELLOW := \033[1;33m
CYAN := \033[1;36m
RESET := \033[0m
CLS := \033[H\033[2J\033[3J

sh: ## start up my own IDE
	@printf "$(CLS)$(YELLOW)"
	@echo ' '
	@echo '  ⠀⠀⠀⠀⠀⠀⠀⠀⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠳⠃⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀'
	@echo '  ⠀⠀⠀⠀⠀⠀⣀⡴⢧⣀⠀⠀⣀⣠⠤⠤⠤⠤⣄⣀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀'
	@echo '  ⠀⠀⠀⠀⠀⠀⠀⠘⠏⢀⡴⠊⠁⠀⠀⠀⠀⠀⠀⠈⠙⠦⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀'
	@echo '  ⠀⠀⠀⠀⠀⠀⠀⠀⣰⠋⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠘⢶⣶⣒⣶⠦⣤⣀⠀⠀'
	@echo '  ⠀⠀⠀⠀⠀⠀⢀⣰⠃⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⣟⠲⡌⠙⢦⠈⢧⠀'
	@echo '  ⠀⠀⠀⣠⢴⡾⢟⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣸⡴⢃⡠⠋⣠⠋⠀'
	@echo '  ⠀⠐⠞⣱⠋⢰⠁⢿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣀⣠⠤⢖⣋⡥⢖⣫⠔⠋⠀⠀⠀'
	@echo '  ⠈⠠⡀⠹⢤⣈⣙⠚⠶⠤⠤⠤⠴⠶⣒⣒⣚⣩⠭⢵⣒⣻⠭⢖⠏⠁⢀⣀⠀⠀⠀⠀'
	@echo '  ⠀⠠⠈⠓⠒⠦⠭⠭⠭⣭⠭⠭⠭⠭⠿⠓⠒⠛⠉⠉⠀⠀⣠⠏⠀⠀⠘⠞⠀⠀⠀⠀'
	@echo '  ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠓⢤⣀⠀⠀⠀⠀⠀⠀⣀⡤⠞⠁⠀⣰⣆⠀⠀⠀⠀⠀⠀'
	@echo '  ⠀⠀⠀⠀⠀⠘⠿⠀⠀⠀⠀⠀⠈⠉⠙⠒⠒⠛⠉⠁⠀⠀⠀⠉⢳⡞⠉⠀⠀⠀⠀⠀'
	@echo '  ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀'
	@printf "$(CYAN)\n  Shoot for the moon. Even if you miss,\n"
	@printf "  you’ll land among the stars\n  — Les Brown\n\n$(RESET)"
	@I=$I bash --init-file $I/etc/bash.rc -i
 
HTML = ~/tmp/html
LUA = $(wildcard *.lua)

Font ?=5
Cols ?=3

~/tmp/%.pdf : %.lua lua.ssh Makefile 
	@echo "pdfing : $@ ... "
	@a2ps -Bj --landscape --line-numbers=1 --highlight-level=normal \
		--prologue=color --borders=no  --right-footer="" --left-footer=""  \
		--pretty-print=./lua.ssh \
    --columns=3 \
    --footer="page %p." -M letter \
    --font-size=$(Font) \
		-o - $< | ps2pdf - $@
	open $@

# ~/tmp/%.pdf : %.jl julia.ssh Makefile 
# 	@echo "pdfing : $@ ... "
# 	@a2ps -Bj --landscape --line-numbers=1 --highlight-level=normal \
# 		--prologue=color --borders=no  --right-footer="" --left-footer=""  \
# 		--pretty-print=./julia.ssh \
#     --footer="page %p." -M letter \
# 		--lines-per-page=100 --columns $(Cols) \
# 		-o - $< | ps2pdf - $@
# 	open $@
#

$(HTML)/%.html: %.lisp # $I/etc/top.html
	@echo "Generating doco for $<"
	@pycco -d $(HTML) $<
	#@cat $I/etc/my.css >> $(HTML)/pycco.css
	#@gawk -v x="$$(cat $I/etc/top.html)" \
    'BEGIN {FS="<h1>"; RS="^$$"} {print $$1 x "<h1>" $$2}' $@ > .$<
	#@mv .$< $@

Font ?=5
Cols ?=3

# ~/tmp/%.pdf : %.lisp Makefile 
# 	@echo "pdfing : $@ ... "
# 	@a2ps -Bj --landscape --line-numbers=1 --highlight-level=normal \
# 		--borders=no --pro=color --right-footer="" --left-footer=""  \
# 		--footer="page %p." -M letter \
# 		--lines-per-page=100 --columns $(Cols) \
# 		-o - $< | ps2pdf - $@
# 	open $@


 SHOW1 = awk 'NF' | feedgnuplot --domain --lines --hardcopy out.png \
          --title   'active learning convergence' \
          --xlabel  'rows seen (n)' \
          --ylabel  'distance' \
          --y2label 'labels' \
          --y2      3 \
          --legend  0 'gap'     \
          --legend  1 'd-gap'   \
          --legend  2 'best-dy' \
          --legend  3 'labels'  \
          --extracmds 'set object 1 rect from screen 0,0 to screen 1,1 fc rgb "white" behind'

Data?=auto93.csv
inc1:
	sbcl --script fri.lisp -s 23 --active  $(Data) | $(SHOW1)
	open out.png
       
