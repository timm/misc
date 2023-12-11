SHELL     := bash 
MAKEFLAGS += --warn-undefined-variables
ifndef LOUD # disable with make LOUD=1
.SILENT: 
endif




help:  ## show help
	awk 'BEGIN {FS = ":.*?## "; print "\nmake [WHAT]" } \
			/^[a-zA-Z_0-9-]+:.*?## / {printf "   \033[36m%-10s\033[0m : %s\n", $$1, $$2} \
			' $(MAKEFILE_LIST)

docs/%.html : %.lua ## make doc
	pycco $^
	echo "$$css" >> docs/pycco.css
	open $@

count1: ## simple count from cli file
	lua count.lua -r mid -f data/diabetes.csv
	lua count.lua -r div -f data/diabetes.csv

count2:  ## simple count from standard input
	cat data/diabetes.csv | lua count.lua -r mid  
	cat data/diabetes.csv | lua count.lua -r div 