# Common make parts
#
# If you include this, make sure the following conditions are met:
#
# a) Your default target is above the include statement, as otherwise
#    our clean-common target becomes your default target
#
# b) Your clean or clean-local or clean-here target depends on our
#    clean-common target so that "make clean" will clean our stuff as
#    well as yours.

TOP_DIR_CANDIDATES =
TOP_DIR_CANDIDATES += .
TOP_DIR_CANDIDATES += ..
TOP_DIR_CANDIDATES += ../..
GIT_VERSION_SH := $(firstword $(wildcard $(addsuffix /git-version.sh,$(TOP_DIR_CANDIDATES))))
GIT_VERSION_H_DUMMY := $(shell $(GIT_VERSION_SH) git-version.h)
TOP_DIR := $(dir $(GIT_VERSION_SH))

ERL_CANDIDATES =
ERL_CANDIDATES += /usr/lib64/erlang/bin
ERL_CANDIDATES += /usr/lib/erlang/bin
ERL_CANDIDATES += /lib64/erlang/bin
ERL_CANDIDATES += /lib/erlang/bin

ifneq (,$(firstword $(wildcard $(addsuffix /erl,$(ERL_CANDIDATES)))))
HAVE_ERL := yes
else
HAVE_ERL := no
endif

MKDIR_P = mkdir -p

.PHONY: clean-common
clean-common:
	rm -f git-version.h
