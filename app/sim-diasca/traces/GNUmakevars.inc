# Prerequisite: TRACES_TOP must be set.


# Project section.

# PROJECT_NAME should be defined on a per-project basis.
ifndef PROJECT_NAME
	PROJECT_NAME = Traces
endif


TRACES_VERSION := 0.3

# PROJECT_VERSION should be defined on a per-project basis.
ifndef PROJECT_VERSION
	PROJECT_VERSION = $(TRACES_VERSION)
endif


ifndef PACKAGE_NAME
	PACKAGE_NAME = traces
endif


ifndef PACKAGE_TOP
	PACKAGE_TOP = $(TRACES_TOP)
endif


TRACES_RELEASE_BASE := Traces-$(PROJECT_VERSION)

TRACES_RELEASE_ARCHIVE_ZIP := $(TRACES_RELEASE_BASE).zip
TRACES_RELEASE_ARCHIVE_BZ2 := $(TRACES_RELEASE_BASE).tar.bz2
TRACES_RELEASE_ARCHIVE_XZ  := $(TRACES_RELEASE_BASE).tar.xz


# Source section.
TRACES_SRC := $(TRACES_TOP)/src

# BEAM path section.
TRACES_BEAM := $(TRACES_TOP)/src

# Include path section.
TRACES_INC = -I$(TRACES_SRC)


DOC_ROOT = $(TRACES_TOP)/../../../doc


INC += $(TRACES_INC)



ifndef VM_TEST_NAME

VM_NAME := traces_debug

endif


ifndef VM_TEST_NAME

VM_TEST_NAME := traces_test

endif



# Overall settings section.


ifndef EXECUTION_TARGET

  # Other possible value: production
  EXECUTION_TARGET=development

endif


ifeq ($(EXECUTION_TARGET),development)

  #$(info Execution target is development)

  ENABLE_TRACES := true


else ifeq ($(EXECUTION_TARGET),production)

  #$(info Execution target is production)

  ENABLE_TRACES := false

else

  $(error Invalid execution target '$(EXECUTION_TARGET)'; expecting either 'development' or 'production')

endif


# Enforce default values:

ifndef ENABLE_TRACES

	ENABLE_TRACES := true

endif



# Tells whether traces will be activated for the next modules to be built.
# By default, they are enabled.
ifeq ($(ENABLE_TRACES),true)

	ENABLE_TRACE_OPT := -DTracingActivated


else

	ENABLE_TRACE_OPT :=

endif



BEAM_DIRS += $(TRACES_BEAM)

ERLANG_COMPILER_OPT += "+{nowarn_unused_function,[{trace_disabled,1},{trace_disabled,2},{trace_disabled,3},{trace_disabled,4},{trace_disabled,5},{test_trace_disabled,1},{test_trace_disabled,2} ]}" $(ENABLE_TRACE_OPT)


# For later reuse in the next layer of the software stack:
TRACES_PLT_FILE := $(TRACES_TOP)/traces.plt


# This is the merged PLT of the level just below in the software stack.
# For Traces, it is the PLT of 'WOOPER':
ifndef PREDECESSOR_PLT
   PREDECESSOR_PLT = $(WOOPER_PLT_FILE)
endif



# WOOPER_TOP may be a symbolic link pointing to the actual WOOPER package to be
# used.
#
WOOPER_TOP = $(TRACES_TOP)/../wooper
