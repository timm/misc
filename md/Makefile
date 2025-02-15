go:
	pandoc $F.md \
		-t beamer  \
		-V "theme:CambridgeUS" \
		-o $F.pdf
	open $F.pdf

slides.html:
	pandoc -t revealjs -s slides.md -o slides.html --variable theme="beige" --css style.css

