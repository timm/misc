.SILENT:

pull: ; git pull

push:
	read -e -p "Why commit> "; git add *; git commit -am "$$x"; git push; git status

%.html : %.md
	cat $<                                             \
	| sed -e 's/;vim.*//' -e 's/^[;\"]//' -e 's/\"$$//' \
	| pandoc -o $@           \
		-s                      \
		-f markdown              \
		--mathjax                 \
		--highlight-style tango    \
		--indented-code-classes=lisp
