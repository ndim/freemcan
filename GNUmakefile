DOXYGEN  ?= doxygen
GIT      ?= git
RST2HTML ?= rst2html
SED      ?= sed
XZ       ?= xz

PACKAGE_TARNAME ?= $(notdir $(PWD))
PACKAGE_VERSION ?= $(shell date -I)
GIT_VERSION ?= $(shell if test -d .git; then git rev-parse --short HEAD; else echo "nongit"; fi)

CLEANFILES =

SUBDIRS = . firmware hostware

.PHONY: all clean ALL
all clean ALL:
	@for subdir in $(SUBDIRS); do \
		if test "x$$subdir" = "x."; then \
			echo $(MAKE) $@-here; \
			$(MAKE) $@-here; \
		else \
			echo $(MAKE) -C "$$subdir" $@; \
			$(MAKE) -C "$$subdir" $@; \
		fi; \
	done

.PHONY: all-here
all-here: README.html

.PHONY: ALL-here
ALL-here: all-here

CLEANFILES += README.html
# Build README.html if rst2html is present. If not, ignore the error.
README.html: README.rst
	if $(RST2HTML) $< > $@.new; then \
		mv -f $@.new $@; \
	else rm -f "$@.new"; fi

.PHONY: clean-here
clean-here:
	rm -f $(CLEANFILES)
	rm -rf dox


TARBASE = $(PACKAGE_TARNAME)-$(PACKAGE_VERSION)-$(GIT_VERSION)

.PHONY: dist
dist:
	test -d .git
	$(GIT) archive --prefix="$(TARBASE)/" HEAD \
	| $(XZ) -c > "$(TARBASE).tar.xz"

Doxyfile: Doxyfile.in
	$(SED) \
		-e 's|@PACKAGE_TARNAME@|$(PACKAGE_TARNAME)|g' \
		-e 's|@PACKAGE_VERSION@|$(PACKAGE_VERSION)|g' \
		-e 's|@PWD@|$(PWD)|g' \
		< $< > $@

.PHONY: dox
dox: Doxyfile
	$(DOXYGEN) $<
