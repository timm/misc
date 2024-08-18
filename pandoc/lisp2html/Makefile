.SILENT:

%.html : %.md
	cat $<             \
	| sed -e '1d' -e 's/^[;\"]//' -e 's/\"$$//'  \
	| pandoc -o $@         \
		-s                    \
		-f markdown            \
		--mathjax               \
		--highlight-style tango  \
		--indented-code-classes=lisp,numberlines
