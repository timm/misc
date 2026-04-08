~/tmp/%.pdf : %.tex Makefile
	tectonic --outdir ~/tmp $< && cp $@ . 

push: ## save to cloud
	@read -p "Reason? " msg; git commit -am "$$msg"; git push; git status


