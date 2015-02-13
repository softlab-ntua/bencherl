COMMON_TOP = .


.PHONY: register-version-in-header info-paths info-settings

#MODULES_DIRS = contrib src doc
MODULES_DIRS = src doc


# To override the 'all' default target with a parallel version:
BASE_MAKEFILE := true


include $(COMMON_TOP)/GNUmakesettings.inc


register-version-in-header:
	@echo "-define( common_version, \"$(COMMON_VERSION)\" )." >> $(VERSION_FILE)


info-paths:
	@echo "BEAM_PATH_OPT = $(BEAM_PATH_OPT)"


info-settings:
	@echo "ERLANG_COMPILER_OPT = $(ERLANG_COMPILER_OPT)"
