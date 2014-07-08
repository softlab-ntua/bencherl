# Prerequisite: WOOPER_TOP must be set.


# Project section.

# PROJECT_NAME should be defined on a per-project basis.
ifndef PROJECT_NAME
	PROJECT_NAME = WOOPER
endif


WOOPER_VERSION := 1.0

# PROJECT_VERSION should be defined on a per-project basis.
ifndef PROJECT_VERSION
	PROJECT_VERSION = $(WOOPER_VERSION)
endif


ifndef PACKAGE_NAME
	PACKAGE_NAME = wooper
endif


ifndef PACKAGE_TOP
	PACKAGE_TOP = $(WOOPER_TOP)
endif


WOOPER_RELEASE_BASE := wooper-$(PROJECT_VERSION)

WOOPER_RELEASE_ARCHIVE_ZIP := $(WOOPER_RELEASE_BASE).zip
WOOPER_RELEASE_ARCHIVE_BZ2 := $(WOOPER_RELEASE_BASE).tar.bz2
WOOPER_RELEASE_ARCHIVE_XZ  := $(WOOPER_RELEASE_BASE).tar.xz


# Source section.
WOOPER_SRC  := $(WOOPER_TOP)/src

# BEAM path section.
WOOPER_BEAM  := $(WOOPER_TOP)

# Include path section.
WOOPER_INC = -I$(WOOPER_SRC)


DOC_ROOT       = $(WOOPER_TOP)/../../../doc
WOOPER_DOC_DIR = $(DOC_ROOT)/web/main/documentation/wooper


INC += $(WOOPER_INC)



ifndef VM_TEST_NAME

VM_NAME := wooper_debug

endif


ifndef VM_TEST_NAME

VM_TEST_NAME := wooper_test

endif


BEAM_DIRS += . $(WOOPER_BEAM)/src $(WOOPER_BEAM)/examples



# Overall settings section.


ifndef EXECUTION_TARGET

  # Other possible value: production
  EXECUTION_TARGET=development

endif


ifeq ($(EXECUTION_TARGET),development)

  #$(info Execution target is development)

  ENABLE_DEBUG=true


else ifeq ($(EXECUTION_TARGET),production)

  #$(info Execution target is production)

  ENABLE_DEBUG=false

else

  $(error Invalid execution target '$(EXECUTION_TARGET)'; expecting either 'development' or 'production')

endif



# By default, this debug mode is enabled:
ifndef ENABLE_DEBUG

	ENABLE_DEBUG = true

endif



# Tells whether the debug mode will be activated for the next WOOPER classes to
# be built (maximum performance versus extended checkings).
ifeq ($(ENABLE_DEBUG),true)

  ENABLE_WOOPER_DEBUG_OPT = -Dwooper_debug

else

  ENABLE_WOOPER_DEBUG_OPT =

endif


ERLANG_COMPILER_OPT += $(ENABLE_WOOPER_DEBUG_OPT)


# For later reuse in the next layer of the software stack:
WOOPER_PLT_FILE := $(WOOPER_TOP)/wooper.plt

# This is the merged PLT of the level just below in the software stack.
# For 'WOOPER', it is the PLT of 'Common':
ifndef PREDECESSOR_PLT
   PREDECESSOR_PLT = $(COMMON_PLT_FILE)
endif

# To use a generic rule to produce it (unless already defined):
ifndef PLT_FILE
		PLT_FILE = $(PACKAGE_TOP)/$(PACKAGE_NAME).plt
endif


# COMMON_TOP may be a symbolic link pointing to the actual Common package to be
# used:
COMMON_TOP = $(WOOPER_TOP)/../common
