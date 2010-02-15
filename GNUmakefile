GIT      ?= git
RST2HTML ?= rst2html
XZ       ?= xz

PACKAGE_TARNAME ?= freemcan
PACKAGE_VERSION ?= $(shell date -I)
GIT_VERSION ?= $(shell if test -d .git; then git rev-parse --short HEAD; else echo "nongit"; fi)

CLEANFILES =

.PHONY: all clean
all clean:
	$(MAKE) $@-here
	$(MAKE) -C firmware $@
	$(MAKE) -C hostware $@

.PHONY: all-here
all-here: README.html

CLEANFILES += README.html
# Build README.html if rst2html is present. If not, ignore the error.
README.html: README.rst
	if $(RST2HTML) $< > $@.new; then \
		mv -f $@.new $@; \
	else rm -f "$@.new"; fi

.PHONY: clean-here
clean-here:
	rm -f $(CLEANFILES)


TARBASE = $(PACKAGE_TARNAME)-$(PACKAGE_VERSION)-$(GIT_VERSION)

.PHONY: dist
dist:
	test -d .git
	$(GIT) archive --prefix="$(TARBASE)/" HEAD \
	| $(XZ) -c > "$(TARBASE).tar.xz"
