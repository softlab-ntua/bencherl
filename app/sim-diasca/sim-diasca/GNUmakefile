SIM_DIASCA_TOP = .


.PHONY: all register-version-in-header                               \
	info-local info-tools info-network info-versions info-compile    \
	clean clean-local clean-host-candidate-files



MODULES_DIRS = src doc


# To override the 'all' default target with a parallel version:
BASE_MAKEFILE = true


# No trace supervisor or graphical output wanted when running all tests from a
# root directory (batch mode vs interactive one):
CMD_LINE_OPT = "--batch"


all:


register-version-in-header:
	@echo "-define( sim_diasca_version, \"$(SIM_DIASCA_VERSION)\" )." >> $(VERSION_FILE)


info-local:
	@echo "BEAM_PATHS = $(BEAM_PATHS)"
	@echo "ARCHIVE_LOCATION = $(ARCHIVE_LOCATION)"


info-tools:
	@echo "ERLANG_INTERPRETER = $(ERLANG_INTERPRETER)"
	@echo "ERLANG_COMPILER = $(ERLANG_COMPILER)"
	@echo "LOGMX = $(LOGMX)"


info-network:
	@echo "FQDN = $(FQDN)"


info-versions:
	@echo "SIM_DIASCA_VERSION = $(SIM_DIASCA_VERSION)"


info-compile:
	@echo "EXECUTION_TARGET          = $(EXECUTION_TARGET)"
	@echo "ENABLE_NATIVE_COMPILATION = $(ENABLE_NATIVE_COMPILATION)"
	@echo "ENABLE_TRACES             = $(ENABLE_TRACES)"
	@echo "ENABLE_DEBUG              = $(ENABLE_DEBUG)"


clean: clean-local


clean-local:
	@find . \( -name '*.dia~' -o -name 'svn-commit.tmp' \) \
	-exec /bin/rm -f '{}' ';'




# When moving source archives here and there, often symbolic links pointing to
# sim-diasca-host-candidates.txt files are replaced by actual copies.
#
# We get rid of them here, see the make-config-links-recurse rule to repopulate
# these files with links.
#
# We do not want to recurse in 'conf' as the original lies there:
#
clean-host-candidate-files:
	@find src -name "$(SIM_DIASCA_HOST_FILE)" -exec /bin/rm -f '{}' ';'


include $(SIM_DIASCA_TOP)/GNUmakesettings.inc
