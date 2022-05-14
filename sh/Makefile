###  ___                                         ____       _____      ______   
### /\_ \                                       /\  _`\    /\  __`\   /\__  _\  
### \//\ \       __      __      _ __    ___    \ \ \L\ \  \ \ \/\ \  \/_/\ \/  
###   \ \ \    /'__`\  /'__`\   /\`'__\/' _ `\   \ \  _ <'  \ \ \ \ \    \ \ \  
###    \_\ \_ /\  __/ /\ \L\.\_ \ \ \/ /\ \/\ \   \ \ \L\ \  \ \ \_\ \    \ \ \ 
###     /\____\\ \____\\ \__/.\_\ \ \_\ \ \_\ \_\   \ \____/   \ \_____\    \ \_\
###     \/____/ \/____/ \/__/\/_/  \/_/  \/_/\/_/    \/___/     \/_____/     \/_/

define BSD_TXT
learnBOT: (BOT = box of tricks). A tiny data minging workbench.
Copyright (c) 2022, Tim Menzies, timm@ieee.org

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
endef
export BSD_TXT

help: ## show help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| sort \
	| awk 'BEGIN {FS = ":.*?## "}; \
               {printf "make \033[36m%-20s\033[0m %s\n", $$1, $$2}'

bye:	## save to github
	git add *;git commit -am save;git push;git status

copyright: ## show copyright
	@echo "$$BSD_TXT"

egok: ## example of "ok"
	 cat data/weatherwrong.csv | python3 ok.py -s

####---------------------------------------------------------------------------
COMMENT= gawk 'BEGIN {"-------------------------------------------------------------------"} \
                 /^[ \t]*$$/ {next} {print "\#\#\#    "$$0} END{print""}'

h1:; @figlet -W -flarry3d     $t | $(COMMENT)
h4:; @figlet -W -fmini $${t%%.*}  | $(COMMENT)
h3:; @figlet -W -fsmall  $${t%%.*}  | $(COMMENT)
h2:; @figlet -W -fcybermedium $t | $(COMMENT)

####---------------------------------------------------------------------------
COLORS = gawk 'BEGIN      {fails=0; print 1;\
                           GREEN= sprintf("%c[1;32m",27); \
                           RED=   sprintf("%c[1;31m",27); \
                           NORM=  sprintf("%c[0m",27) } \
          gsub(/PASS/,"") {$$0 = GREEN "PASS" NORM $$0 } \
          gsub(/FAIL/,"") {$$0 = RED   "FAIL" NORM $$0; fails++ } \
                          {print} \
          END             {print "\n" RED "ERRORS ==> " fails " " NORM;\
                           exit(fails)}' 

tests:
	@$(MAKE) testsgo |  $(COLORS)

testsgo:
	@echo "PASS asdas"
	@echo "FAIL asdas"
