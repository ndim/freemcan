-include local.mk

AWK       ?= awk
DOXYGEN   ?= doxygen
GIT       ?= git
GZIP      ?= gzip
NEATO     ?= neato
SED       ?= sed
SLOCCOUNT ?= sloccount
XZ        ?= xz

PACKAGE_TARNAME ?= $(notdir $(shell pwd))
PACKAGE_VERSION ?= $(shell date -I)
GIT_VERSION ?= $(shell if test -d .git; then git rev-parse --short HEAD; else echo "nongit"; fi)

CLEANFILES =

SUBDIRS = . firmware hostware

ifneq ($(shell for f in {,/usr}/lib{,64}/erlang/lib/erl_interface-*/lib/libei*; do if test -f "$$f"; then echo xyz; fi; done),)
SUBDIRS_emulator = emulator
endif
SUBDIRS += $(SUBDIRS_emulator)

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

.PHONY: all-here
all-here:

# Legacy target
.PHONY: ALL
ALL: all

# Legacy target
.PHONY: ALL-here
ALL-here: all-here

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
dox-files: dox/built/firmware/firmware-fsm.png

dox/built/firmware/firmware-fsm.png: include/firmware-fsm.png
	mkdir -p "$(@D)"
	cp $< $@

dox/built/%.png: include/%.neato
	mkdir -p "$(@D)"
	$(NEATO) -o "$@" -Tpng $<

RSYNC_USER ?= rsync_user
RSYNC_HOST ?= rsync-host.example.com
RSYNC_USERHOST ?= $(RSYNC_USER)@$(RSYNC_HOST)
RSYNC_SUBDIR ?= sub/dir/
RSYNC_OPTS ?=

.PHONY: upload-dox
upload-dox: dox
	chmod -R a+rX dox/html
	rsync -avz $(RSYNC_OPTS) dox/html/ $(RSYNC_USERHOST):$(RSYNC_SUBDIR)

.PHONY: sloccount
sloccount:
	@$(SLOCCOUNT) emulator/ firmware/ hostware/ \
	| $(AWK) 'BEGIN { verb=0; } /^SLOCCount/ { verb=0; } /^SLOC/ { verb=1; } /^SLOCCount/ { verb=0; } (verb) { print; }'; \
	echo; echo "Statistics generated using David A. Wheeler's 'SLOCCount'."
