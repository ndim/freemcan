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

TOP_DIR := $(shell for d in . .. ../..; do if test -f "$$d/git-version.sh"; then echo "$$d"; exit; fi; done; exit 1)
FOO_1 := $(shell $(TOP_DIR)/git-version.sh git-version.h)

HAVE_ERL := $(shell for f in {,/usr}/lib{,64}/erlang/bin/erl; do if test -e "$$f"; then echo yes; exit; fi; done; echo no)

MKDIR_P = mkdir -p

.PHONY: clean-common
clean-common:
	rm -f git-version.h
