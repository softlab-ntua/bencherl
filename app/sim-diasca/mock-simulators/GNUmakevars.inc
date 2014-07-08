# Prerequisite: MOCK_SIMULATORS_TOP must be set.


# Project section.

# PROJECT_NAME should be defined on a per-project basis.
ifndef PROJECT_NAME
	PROJECT_NAME = Mock-Simulators
endif


MOCK_SIMULATORS_VERSION := 0.1

# PROJECT_VERSION should be defined on a per-project basis.
ifndef PROJECT_VERSION
	PROJECT_VERSION = $(MOCK_SIMULATORS_VERSION)
endif


ifndef PACKAGE_NAME
	PACKAGE_NAME = mock-simulators
endif


ifndef PACKAGE_TOP
	PACKAGE_TOP = $(MOCK_SIMULATORS_TOP)
endif


MOCK_SIMULATORS_RELEASE_BASE := Mock-Simulators-$(PROJECT_VERSION)

MOCK_SIMULATORS_RELEASE_ARCHIVE_ZIP := $(MOCK_SIMULATORS_RELEASE_BASE).zip
MOCK_SIMULATORS_RELEASE_ARCHIVE_BZ2 := $(MOCK_SIMULATORS_RELEASE_BASE).tar.bz2


# Source section.
MOCK_SIMULATORS_SRC := $(MOCK_SIMULATORS_TOP)/ssi-test/src


# BEAM path section.
MOCK_SIMULATORS_BEAM := $(MOCK_SIMULATORS_TOP)/ssi-test/src

# Include path section.
MOCK_SIMULATORS_INC = -I$(MOCK_SIMULATORS_SRC)


DOC_ROOT = $(MOCK_SIMULATORS_TOP)/doc


INC += $(MOCK_SIMULATORS_INC)



ifndef VM_TEST_NAME

VM_NAME := mock-simulators_debug

endif


ifndef VM_TEST_NAME

VM_TEST_NAME := mock-simulators_test

endif


BEAM_DIRS += $(MOCK_SIMULATORS_BEAM)

# Actually not used, as defined on a per-simulator basis:

# For later reuse in the next layer of the software stack:
#MOCK_SIMULATORS_PLT_FILE := $(MOCK_SIMULATORS_TOP)/mock-simulators.plt

# This is the merged PLT of the level just below in the software stack.
# For Mock Simulators, it is the PLT of Sim-Diasca:
#ifndef PREDECESSOR_PLT
#   PREDECESSOR_PLT = $(SIM_DIASCA_PLT_FILE)
#endif


SIM_DIASCA_TOP = $(MOCK_SIMULATORS_TOP)/../sim-diasca
