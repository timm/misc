TEX = verifai-ase

all: ## full build: reruns + bibtex until fixpoint
	tectonic $(TEX).tex

fast: ## single pass, no reruns
	tectonic -r 0 $(TEX).tex

v2: ## old llncs version
	tectonic verifai-v2.tex

~/tmp/%.pdf : %.md Makefile
	pandoc $< -o $@  --pdf-engine=tectonic  -t beamer
	#@open $@
