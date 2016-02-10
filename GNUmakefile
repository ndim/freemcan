-include local.mk

DOXYGEN   ?= doxygen
GIT       ?= git
GZIP      ?= gzip
NEATO     ?= neato
SLOCCOUNT ?= sloccount
XZ        ?= xz

PACKAGE_TARNAME ?= $(notdir $(shell pwd))
PACKAGE_VERSION ?= $(shell date -I)
GIT_VERSION ?= $(shell if test -d .git; then $(GIT) rev-parse --short HEAD; else echo "nongit"; fi)

CLEANFILES =

SUBDIRS =
SUBDIRS += .
SUBDIRS += firmware
SUBDIRS += hostware

ifneq ($(shell for f in {,/usr}/lib{,64}/erlang/lib/erl_interface-*/lib/libei.*; do if test -f "$$f"; then echo xyz; fi; done),)
SUBDIRS += emulator
endif

.PHONY: all clean
all clean:
	@set -e; for subdir in $(SUBDIRS); do \
		if test "x$$subdir" = "x."; then \
			echo $(MAKE) $@-here; \
			$(MAKE) $@-here; \
		else \
			echo $(MAKE) -C "$$subdir" $@; \
			$(MAKE) -C "$$subdir" $@; \
		fi; \
	done

include common.mk

.PHONY: all-here
all-here:

# Legacy target
.PHONY: ALL
ALL: all

# Legacy target
.PHONY: ALL-here
ALL-here: all-here

.PHONY: clean-here
clean-here: clean-common
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
	$(MKDIR_P) dox/html
	$(DOXYGEN) -w html \
		dox/html/default-header.html \
		dox/html/default-footer.html \
		dox/html/default-stylesheet.css

.PHONY: dox
dox: Doxyfile dox-files
	$(DOXYGEN) $<

.PHONY: dox-files
dox-files: dox/built/firmware/firmware-fsm.png

dox/built/firmware/firmware-fsm.png: include/firmware-fsm.png
	$(MKDIR_P) $(@D)
	cp $< $@

dox/built/%.png: include/%.neato
	$(MKDIR_P) $(@D)
	$(NEATO) -o "$@" -Tpng $<

RSYNC_USER ?= rsync_user
RSYNC_HOST ?= rsync-host.example.com
RSYNC_USERHOST ?= $(RSYNC_USER)@$(RSYNC_HOST)
RSYNC_SUBDIR ?= sub/dir/
RSYNC_OPTS ?=
RSYNC      ?=

.PHONY: upload-dox
upload-dox: dox
	chmod -R a+rX dox/html
	$(RSYNC) -avz $(RSYNC_OPTS) dox/html/ $(RSYNC_USERHOST):$(RSYNC_SUBDIR)

.PHONY: sloccount
sloccount:
	@$(SLOCCOUNT) emulator/ firmware/ hostware/ \
	| $(AWK) 'BEGIN { verb=0; } /^SLOCCount/ { verb=0; } /^SLOC/ { verb=1; } /^SLOCCount/ { verb=0; } (verb) { print; }'; \
	echo; echo "Statistics generated using David A. Wheeler's 'SLOCCount'."
