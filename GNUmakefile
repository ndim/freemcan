-include local.mk

AWK       ?= awk
DOXYGEN   ?= doxygen
GIT       ?= git
GZIP      ?= gzip
NEATO     ?= neato
RST2HTML  ?= rst2html
SED       ?= sed
SLOCCOUNT ?= sloccount
XZ        ?= xz

PACKAGE_TARNAME ?= $(notdir $(shell pwd))
PACKAGE_VERSION ?= $(shell date -I)
GIT_VERSION ?= $(shell if test -d .git; then git rev-parse --short HEAD; else echo "nongit"; fi)

CLEANFILES =

SUBDIRS = . firmware hostware emulator

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
	rm -rf dox Doxyfile


TARBASE = $(PACKAGE_TARNAME)-$(PACKAGE_VERSION)-g$(GIT_VERSION)

.PHONY: dist
dist:
	test -d .git
	$(GIT) archive --prefix="$(TARBASE)/" HEAD \
	| $(XZ) -c > "$(TARBASE).tar.xz"

Doxyfile: Doxyfile.in
	PWD="$$(pwd)"; \
	$(SED) \
		-e 's|@PACKAGE_TARNAME@|$(PACKAGE_TARNAME)|g' \
		-e 's|@PACKAGE_VERSION@|$(PACKAGE_VERSION)-g$(GIT_VERSION)|g' \
		-e "s|@PWD@|$${PWD}|g" \
		< $< > $@
	mkdir -p dox/html
	$(DOXYGEN) -w html \
		dox/html/default-header.html \
		dox/html/default-footer.html \
		dox/html/default-stylesheet.css

.PHONY: dox
dox: Doxyfile dox-files
	$(DOXYGEN) $<

.PHONY: dox-files
dox-files: dox/built/firmware-states.png

dox/built/%.png: include/%.neato
	mkdir -p "$(@D)"
	$(NEATO) -o "$@" -Tpng $<

RSYNC_USER ?= rsync_user
RSYNC_HOST ?= rsync-host.example.com
RSYNC_USERHOST ?= $(RSYNC_USER)@$(RSYNC_HOST)
RSYNC_SUBDIR ?= sub/dir/

.PHONY: upload-dox
upload-dox: dox
	chmod -R a+rX dox/html
	rsync -avz dox/html/ $(RSYNC_USERHOST):$(RSYNC_SUBDIR)

.PHONY: sloccount
sloccount:
	@$(SLOCCOUNT) emulator/ firmware/ hostware/ \
	| $(AWK) 'BEGIN { verb=0; } /^SLOCCount/ { verb=0; } /^SLOC/ { verb=1; } /^SLOCCount/ { verb=0; } (verb) { print; }'; \
	echo; echo "Statistics generated using David A. Wheeler's 'SLOCCount'."
