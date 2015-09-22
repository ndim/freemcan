# Common make parts

FOO_1 := $(shell ../git-version.sh git-version.h)
CLEANFILES += git-version.h

HAVE_ERL := $(shell for f in {,/usr}/lib{,64}/erlang/bin/erl; do if test -e "$$f"; then echo yes; exit; fi; done; echo no)

MKDIR_P = mkdir -p
