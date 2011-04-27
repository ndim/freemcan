# Common make parts

FOO_1 := $(shell ../git-version.sh git-version.h)
CLEANFILES += git-version.h

MKDIR_P = mkdir -p
