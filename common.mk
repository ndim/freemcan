# Common make parts

TOP_DIR := $(shell for d in . .. ../..; do if test -f "$$d/git-version.sh"; then echo "$$d"; exit; fi; done; exit 1)
FOO_1 := $(shell $(TOP_DIR)/git-version.sh git-version.h)
CLEANFILES += git-version.h

HAVE_ERL := $(shell for f in {,/usr}/lib{,64}/erlang/bin/erl; do if test -e "$$f"; then echo yes; exit; fi; done; echo no)

MKDIR_P = mkdir -p
