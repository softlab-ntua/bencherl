MOCK_SIMULATORS_TOP = .


.PHONY: register-version-in-header lean-host-candidate-files


MODULES_DIRS = ssi-test soda-test city-example doc


# To override the 'all' default target with a parallel version:
BASE_MAKEFILE = true


register-version-in-header:
	@echo "-define( mock_simulators_version, \"$(MOCK_SIMULATORS_VERSION)\" )." >> $(VERSION_FILE)


# Far simpler to duplicate and adapt the rule, rather then fixing all *_TOP
# variables, recursing and al:
#
# (could have been: '$(MAKE) -f $(SIM_DIASCA_TOP)/GNUmakefile $<')
clean-host-candidate-files:
	@find . -name "$(SIM_DIASCA_HOST_FILE)" -exec /bin/rm -f '{}' ';'


include $(MOCK_SIMULATORS_TOP)/GNUmakesettings.inc
