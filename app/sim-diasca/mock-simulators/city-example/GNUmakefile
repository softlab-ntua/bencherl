MOCK_SIMULATORS_TOP = ..


MODULES_DIRS = src doc


PACKAGE_NAME := city-example
PACKAGE_TOP  := $(MOCK_SIMULATORS_TOP)/$(PACKAGE_NAME)


# This is the merged PLT of the level just below in the software stack.
# For Mock Simulators, it is the PLT of Sim-Diasca:
ifndef PREDECESSOR_PLT
   PREDECESSOR_PLT = $(SIM_DIASCA_PLT_FILE)
endif


include $(MOCK_SIMULATORS_TOP)/GNUmakesettings.inc
