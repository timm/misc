help: ## show help.
	@gawk  '\
		BEGIN {FS = ":.*?##"; \
           printf "\nUsage:\n  make \033[36m<target>\033[0m\n\ntargets:\n"} \
    /^[~a-z0-9A-Z_%\.\/-]+:.*?##/ { \
           printf("  \033[36m%-15s\033[0m %s\n", $$1, $$2) | "sort " } \
		'$(MAKEFILE_LIST)

push: ## save to cloud
	@read -p "Reason? " msg; git commit -am "$$msg"; git push; git status

sh: ## start up my own IDE
	@bash --init-file .bashrc -i
