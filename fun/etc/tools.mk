#### set up #################################
SHELL:= bash#
MAKEFLAGS += --silent
MAKEFLAGS += --no-buildin-rules
MAKEFLAGS += --warn-undefined-variables

##### paths #################################
PATH    := $(Built)/bin:$(PATH)#
AWKPATH := $(Built)/lib:.:$(AWKPATH)#
Awk     = AWKPATH=$(AWKPATH) gawk -f lib.awk -f #

#### directories ####################
# places to get things
Repo ?= $(PWD)
Src   = $(Repo)/src
Test  = $(Repo)/test
Md    = $(Repo)/docs

# places to put things
Build ?= $(PWD)
Bin   = $(Built)/bin#
Doc   = $(Built)/doc#
Lib   = $(Built)/lib#

##### files #################################

Funs0=$(shell find $(Src) $(Test) -type f -name '*.fun')
Awks0=$(shell find $(Src) $(Test) -type f -name '*.awk')

Awks=$(subst .fun,.awk,           \
       $(subst $(Src),$(Lib),     \
         $(subst $(Test),$(Lib), \
           $(Funs0) $(Awks0))))

Docs=$(subst .fun,.md,           \
       $(subst $(Src),$(Doc),     \
         $(shell find $(Src) -type f -name '*.fun')))

test1000:
	echo $(Docs)

##### transforms  #################################

$(Lib)/%.awk : $(Test)/%.fun 
	  gawk "$$PARSE" $< | gawk "$$CODE" > $@
 	 
$(Lib)/%.awk : $(Src)/%.fun 
	  gawk "$$PARSE" $< | gawk "$$CODE" > $@

$(Lib)/%.awk : $(Test)/%.awk; cp $< $@
$(Lib)/%.awk : $(Src)/%.awk ; cp $< $@

$(Doc)/%.md  : $(Src)/%.fun
	gawk "$$PARSE" $< \
	| gawk -vhead="$(basename $(notdir $@)).fun" \
               -vtop="$$BUTTONS" \
               "$$DOC" > $@

#### installing tools #########################
.PHONY: install

ifeq ($(OS),Windows_NT)
install: ## Install support tools (only needs to be run once) 
	echo "can someone tell me how to install on windoze?"
else 
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
install:	
	sudo -H apt install gawk grip 
endif
ifeq ($(UNAME_S),Darwin)
install:	
	brew install gawk 
	brew install grip
endif
endif

#### building this system #########################
.PHONY: reset zap all dirs 

all  : dirs $(Awks) $(Docs) ## Build: build everything

zap : ## Build: delete all the built stuff
	rm -rf $(Built)

reset: zap all ## Build: delete, then rebuild everything; i.e. zap+all

dirs: 
	mkdir -p $(Bin) $(Doc) $(Lib) $(Src) $(Md) $(Test)

publish: $(Docs) ## Push doc files to the web
	cp $(Doc)/* $(Md)
	git add $(Md)/*
	git commit -am "new docs"
	git push

#### github tools  ##################################
.PHONY: gfig put get

gfig: 
	git config --global credential.helper cache
	git config credential.helper 'cache --timeout=3600'

put: gfig ## Git: push all changes
	git commit -am saving; git push; git status

get: gfig ## Git: pull new material from web
	git pull

#### scripts ##################################
## my translation scripts

# identifying code parts
define PARSE
/^@include/              { print "CODE "$$0; next }    
/^(func|BEGIN|END).*}$$/  { print "CODE "$$0; next }    
/^(func|BEGIN|END)/,/^}/ { print "CODE "$$0; next }  
                         { print "DOC " $$0}      
endef
export PARSE

# handle the code parts
define CODE
function prep(s) {
    print gensub(/\.([^0-9])([a-zA-Z0-9_]*)/,
                  "[\"\\1\\2\"]","g",s) }
sub(/^DOC /,"#")         { print; next }
                         { gsub(/(CODE |[ \t]*$$)/,"")   }
/^@include/              { prep($$0); next }
/^(func|BEGIN|END).*}$$/ { prep($$0); next }
/^(func|BEGIN|END)/,/^}/ { prep($$0); next }
                         { print "# " $$0  } 
endef
export CODE

## documentation for CODE and DOC parts
define DOC
sub(/^CODE /,"") { if(!Code) print "```awk";
                    Code=1;
                    print sprintf("%4s.  ",++N) $$0;
                    next }
sub(/^DOC /,"")  { if( Code) print "```";
                   Code=0 }
BEGIN            { print  "---\ntitle: " head "\n---\n\n"
                   print  top   "<br>\n\n# " head "\n\n" }
NR < 3           { next }
                 { print }
END              { if (Code) print "```";  } 
endef
export DOC

## button bar (top of each page)
define BUTTONS
<button class="button button1"> 
	<a href="/fun/index">home</a> 
</button><button class="button button2"> 
	<a href="/fun/INSTALL">install</a>
</button><button class="button button1"> 
	<a href="/fun/ABOUT">doc</a> 
</button><button class="button button2"> 
	<a href="http://github.com/timm/fun/issues">discuss</a> 
</button><button class="button button1">
	<a href="/fun/LICENSE">license</a> 
</button>
endef
export BUTTONS

##### self-documenting code #################################
.DEFAULT_GOAL = help#
.PHONY:  help
help : $(Repo)/etc/tools.mk ##  Show options
	gawk 'BEGIN {FS="[ \t]*:.*##[ \t]*"}  \
	  NF==2 && $$0 ~ /^[a-zA-Z]/ { printf   \
	    "\033[36m%-15s\033[0m %s\n","make " $$1,$$2}'  \
		$(MAKEFILE_LIST) 


define HELLO
  ______
 `""-.  `````-----.....__
      `.  .      .       `-.
        :     .     .       `.
  ,     :   .    .          _ :
 : `.   :                  (@) `._   
  `. `..'     .     =`-.       .__}  
    ;     .        =  ~  :     .-"
  .' .'`.   .    .  =.-'  `._ .'
 : .'   :               .   .'
  '   .'  .    .     .   .-'
jgs .'____....----''.'=.'   simpler.ai v0.1
    ""             .'.'     http://menzies.us/simplerai
                ''"'`

endef
export HELLO
