CLEANFILES =

SUBDIRS = atmega

# The code comparison is just for hacking, so you can build
# that manually if you need that.
# SUBDIRS += code-comparison

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
all-here:

.PHONY: ALL-here
ALL-here: all-here

.PHONY: clean-here
clean-here:
	rm -f $(CLEANFILES)
