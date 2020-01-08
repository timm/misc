## Usage: 
##   make help

##### directories #################################
What  = fun#

# places to get things
Repo  = $(shell git rev-parse --show-toplevel)/$(What)#
Src   = $(Repo)/src
Test  = $(Repo)/test
Md    = $(Repo)/docs

# places to put things
Built = $(HOME)/opt/$(What)#
Bin   = $(Built)/bin#
Doc   = $(Built)/docs#
Lib   = $(Built)/lib#

##### tools #################################
include $(Repo)/etc/tools.mk

