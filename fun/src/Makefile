#### set up #################################
What  = fun#
.DEFAULT_GOAL = help#
SHELL:= bash#
MAKEFLAGS += --silent
MAKEFLAGS += --no-buildin-rules
MAKEFLAGS += --warn-undefined-variables
.PHONY: help dirs reset zap ok

##### directories #################################
# places to get things
Repo  = $(shell git rev-parse --show-toplevel)/$(What)#
Src   = $(Repo)/src
Test  = $(Repo)/test
Md    = $(Repo)/docs

# places to put things
Built = $(HOME)/opt/$(What)#
Bin   = $(HOME)/opt/$(What)/bin#
Doc   = $(HOME)/opt/$(What)/docs#
Lib   = $(HOME)/opt/$(What)/lib#

##### paths #################################
PATH    := $(Built)/bin:$(PATH)#
AWKPATH := $(Built)/lib:.:$(AWKPATH)#
Awk     = AWKPATH=$(AWKPATH) gawk -f lib.awk -f #

##### files #################################

Funs0=$(shell find $(Src) $(Test) -type f -name '*.fun')
Awks0=$(shell find $(Src) $(Test) -type f -name '*.awk')

Awks=$(subst .fun,.awk,           \
         $(subst $(Src),$(Lib),     \
             $(subst $(Test),$(Lib), \
                $(Funs0) $(Awks0))))

Docs=$(subst .fun,.md,           \
         $(subst $(Src),$(Doc),     \
                $(Funs0)))


test1000:
	echo $(Docs)

$(Lib)/%.awk : $(Test)/%.fun 
	  gawk "$$PARSE" $< | gawk "$$CODE" > $@
 	 
$(Lib)/%.awk : $(Src)/%.fun 
	  gawk "$$PARSE" $< | gawk "$$CODE" > $@

$(Lib)/%.awk : $(Test)/%.awk; cp $< $@
$(Lib)/%.awk : $(Src)/%.awk ; cp $< $@

$(Doc)/%.md  : $(Src)/%.fun
  	gawk "$$PARSE" $< \
	| gawk -vhead="$(basename $<)" \
               -vtop="$$BUTTONS" \
               "$$DOC" > $@

#### building code #########################
reset: zap ok ## rebuilds everything

zap : ## delete all built
	rm -rf $(Built)

ok  : dirs $(Awks) $(Docs) ## ensure all files are built 

dirs: 
	mkdir -p $(Bin) $(Doc) $(Lib) $(Src) $(Md) $(Test)

help : Makefile ## help
	echo ""; echo "usage: "
	gawk 'BEGIN {FS="[ \t]*:.*##[ \t]*"}  \
	  NF==2 && $$0 ~ /^[a-zA-Z]/ { printf   \
           "  \033[36m%-15s\033[0m %s\n","make " $$1,$$2}'  $< 

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

#%: echo gawk --source 'BEGIN { $* }'


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
b0=</button>
b1=<button class="button button1">
b2=<button class="button button2">
define BUTTONS
$$b1     <a href="/fun/index">home</a> 
$$b0$$b2 <a href="/fun/INSTALL">install</a>
$$b0$$b1 <a href="/fun/ABOUT">doc</a> 
$$b0$$b2 <a href="http://github.com/timm/fun/issues">discuss</a> 
$$b0$$b1 <a href="/fun/LICENSE">license</a> 
$$b0
endef
export BUTTONS

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


