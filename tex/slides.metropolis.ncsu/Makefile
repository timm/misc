%.pdf : %.md Makefile
	pandoc $< -t beamer --pdf-engine=pdflatex -H header.tex -V listings=true -V minted=true --o $@

push:
	git commit -am saving; git push
