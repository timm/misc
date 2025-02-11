%.pdf : %.md
	pandoc $^ -t beamer --pdf-engine=pdflatex -H header.tex -o $@
