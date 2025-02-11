%.pdf : %.md
	pandoc $^ -t beamer --pdf-engine=pdflatex -H header.tex -o $@

push:
	git commit -am saving; git push
