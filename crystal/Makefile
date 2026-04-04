# vim: ts=2 sw=2 sts=2 et  :
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

~/tmp/%.html: %.cr
	@pycco -l ruby -d $(dir $@) $<
	@echo 'p {text-align: right}' >> $(dir $@)pycco.css
	@gawk -v x="$$(cat top.html)" 'BEGIN{FS="<h1>"; RS="jqz"} {print $$1 x $$2}' $@ > .$<
	@mv .$< $@
