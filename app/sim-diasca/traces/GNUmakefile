TRACES_TOP = .


.PHONY: all register-version-in-header info-traces


MODULES_DIRS = src doc #conf

# To override the 'all' default target with a parallel version:
BASE_MAKEFILE = true


# No trace supervisor or graphical output wanted when running all tests from a
# root directory (batch mode vs interactive one):
CMD_LINE_OPT = "--batch"


# Default target:
all:


register-version-in-header:
	@echo "-define( traces_version, \"$(TRACES_VERSION)\" )." >> $(VERSION_FILE)


info-traces:
	@echo "ENABLE_TRACE_OPT = $(ENABLE_TRACE_OPT)"


include $(TRACES_TOP)/GNUmakesettings.inc
