RST2HTML ?= rst2html
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

