~/tmp/%.pdf : %.md Makefile
	pandoc $< -o $@  --pdf-engine=tectonic  -t beamer
	#@open $@
