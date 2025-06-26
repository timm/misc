# Default is show help; e.g.
#
#    make 
#
# prints the help text.

SHELL     := bash
MAKEFLAGS += --warn-undefined-variables
.SILENT:

Top=$(shell git rev-parse --show-toplevel)
Tmp  ?= $(HOME)/tmp

help      :  ## show help
	gawk -f $(Top)/etc/help.awk $(MAKEFILE_LIST) 

pull    : ## download
	git pull

push    : ## save
	read -ep "Why this push? " x; git commit -am "$$x"; git push; git status

$(Tmp)/%.pdf: %.py  ## make doco: .py ==> .pdf
	mkdir -p ~/tmp
	echo "pdf-ing $@ ... "
	a2ps                 \
		-br                 \
    --chars-per-line=70 \
		--file-align=fill      \
		--line-numbers=1        \
		--pro=color               \
		--left-title=""            \
		--borders=no             \
	    --left-footer="$<  "               \
	    --right-footer="page %s. of %s#"               \
		--columns 2                 \
		--portrait \
		-M letter                     \
	  -o	 $@.ps $<
	ps2pdf $@.ps $@; rm $@.ps
	open $@

zapr:
	(echo "#gain _2 diff _4 height _6 test _8 evals _10 yes _12  file _14"; cat ~/tmp/zap.*) | column -t

zaps: ../../moot/optimize/[mpch]*/*.csv
	$(foreach f, $^, $(MAKE) F=$f zap;)

F=../data/auto93.csv
zap:
	./zap.py  --zap $F  | tee ~/tmp/$@.$(notdir $F)

html:
	pdoc --logo ort.png -o ~/tmp/x ort.py

scikitInstall:
	python3 -m venv sklearn-env
	- source sklearn-env/bin/activate
	- pip3 install -U scikit-learn
