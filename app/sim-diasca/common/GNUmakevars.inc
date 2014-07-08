# Prerequisite: COMMON_TOP must be set.


# Project section.


# PROJECT_NAME should be defined on a per-project basis.
ifndef PROJECT_NAME
	PROJECT_NAME = Ceylan-common
endif


COMMON_VERSION := 0.1

# PROJECT_VERSION should be defined on a per-project basis.
ifndef PROJECT_VERSION
	PROJECT_VERSION = $(COMMON_VERSION)
endif



ifndef PACKAGE_NAME
	PACKAGE_NAME = common
endif


ifndef PACKAGE_TOP
	PACKAGE_TOP = $(COMMON_TOP)
endif


ARCHIVE_ROOT := $$HOME/Archives/$(PROJECT_NAME)


ARCHIVE_FILE := $(ARCHIVE_ROOT)/`date +'%Y%m%d'`-$(PROJECT_NAME).tar.xz

VCS_ARCHIVE_FILE := $(ARCHIVE_ROOT)/`date +'%Y%m%d'`-$(PROJECT_NAME).git-bundle


ARCHIVE_BRANCH_FILE = $(ARCHIVE_ROOT)/`date +'%Y%m%d'`-$(PROJECT_NAME)-branch-$(shell basename `pwd`).tar.xz


SNAPSHOT_ARCHIVE_FILE := $(ARCHIVE_ROOT)/`date +'%Y%m%d'`-$(PROJECT_NAME)-snapshot.tar.xz


PROJECT_RELEASE_BASE        := $(PROJECT_NAME)-$(PROJECT_VERSION)

PROJECT_RELEASE_ARCHIVE_ZIP := $(PROJECT_RELEASE_BASE).zip
PROJECT_RELEASE_ARCHIVE_BZ2 := $(PROJECT_RELEASE_BASE).tar.bz2
PROJECT_RELEASE_ARCHIVE_XZ  := $(PROJECT_RELEASE_BASE).tar.xz




# Installation section.


# If wanting to install that package, following installation prefix is used.
#
# INSTALLATION_PREFIX should be defined on a per-project basis, or be overridden
# on the command line (ex: 'make install INSTALLATION_PREFIX=/tmp'):

ifndef INSTALLATION_PREFIX
	INSTALLATION_PREFIX = $$HOME/Software
endif


PACKAGE_INSTALLATION_PREFIX = $(INSTALLATION_PREFIX)/$(PACKAGE_NAME)


# Could rely on the permissions instead:
EXEC_TO_INSTALL := `find $(PACKAGE_TOP) -name '*.sh' -a ! -name '*.exe'`


BEAMS_TO_INSTALL := `find $(PACKAGE_TOP) -name examples -prune -o \( -name '*.beam' -a ! -name '*_test.beam' -a ! -name 'class_Test*.beam' \) -a -print`

SOURCES_TO_INSTALL := `find $(PACKAGE_TOP) -name examples -prune -o \( -name '*.erl' -a ! -name '*_test.erl' -a ! -name 'class_Test*.erl' \) -a -print`


EXAMPLES_TO_INSTALL_BEAMS := `[ ! -d examples ] || find $(PACKAGE_TOP)/examples -name '*.beam'`

EXAMPLES_TO_INSTALL_SRC := `[ ! -d examples ] || find $(PACKAGE_TOP)/examples -name '*.erl'`


INCLUDES_TO_INSTALL := `find $(PACKAGE_TOP) -name '*.hrl'`


TESTS_TO_INSTALL_BEAMS := `find $(PACKAGE_TOP) -name '*_test.beam' -o -name 'class_Test*.beam'`

TESTS_TO_INSTALL_SRC := `find $(PACKAGE_TOP) -name '*_test.erl' -o -name 'class_Test*.erl'`


DOC_TO_INSTALL := `find $(PACKAGE_TOP) -name tmp-rst -prune -o \( -type f -a \( -name '*.pdf' -o -name '*.html' \) \)|grep -v tmp-rst`



# Source section.
COMMON_SRC := $(COMMON_TOP)/src

# BEAM path section.
COMMON_BEAM := $(COMMON_TOP)/src

# Include path section.
COMMON_INC = -I$(COMMON_SRC)

# Contributions path section:
COMMON_CONTRIB = $(COMMON_TOP)/contrib


INC += $(foreach dir,$(BEAM_DIRS),-I$(dir))


# To detect the sole root makefile:
ifndef ROOT_MAKEFILE

	   ROOT_MAKEFILE = false

endif


# Unless specified otherwise, a makefile is not the one used as the base of a
# given layer (hence, for example, it is not expected to build in parallel):
ifndef BASE_MAKEFILE

	   BASE_MAKEFILE = false

endif



# Overall settings section.


ifndef EXECUTION_TARGET

  # Other possible value: production
  EXECUTION_TARGET = development

endif


# We have to define a symbol specific to the 'production' execution target
# (exec_target_is_production) instead of relying only on a execution_target
# variable which could be set either to 'development' or 'production', as the
# Erlang preprocessor allows only to test whether a symbol is defined (with
# -ifdef), we cannot test its value.
#
# So code is to use '-ifdef(target_production_exec)' instead of something like
# '-ifdef(execution_target,production)'.

ifeq ($(EXECUTION_TARGET),development)


  #$(info Execution target is development)

  ENABLE_DEBUG = true
  ENABLE_NATIVE_COMPILATION = false

  # No exec_target_is_production defined here:
  ERLANG_COMPILER_EXEC_TARGET_OPT =

  ERLANG_COMPILER_DEBUG_OPT = -Ddebug_mode_is_enabled


else ifeq ($(EXECUTION_TARGET),production)


  #$(info Execution target is production)

  ENABLE_DEBUG = false

  # Native compilation may be enabled - with care:
  #ENABLE_NATIVE_COMPILATION = true

  # Problem with orddict:erase/2; lists:fold/3, etc., thus switched off:
  ENABLE_NATIVE_COMPILATION = false

  ERLANG_COMPILER_EXEC_TARGET_OPT = -Dexec_target_is_production
  ERLANG_COMPILER_DEBUG_OPT =

else


  $(error Invalid execution target '$(EXECUTION_TARGET)'; expecting either 'development' or 'production')


endif


# Enforce default values:


ifndef ENABLE_DEBUG

	ENABLE_DEBUG = true

endif


ifndef ENABLE_NATIVE_COMPILATION

	ENABLE_NATIVE_COMPILATION = false

endif


# Apply settings:

# Tells which set of settings should be applied (allows for conditional code):
ERLANG_COMPILER_EXECUTION_TARGET_OPT += -Dexecution_target=$(EXECUTION_TARGET)

ifeq ($(ENABLE_NATIVE_COMPILATION),true)
	ERLANG_COMPILER_NATIVE_COMPILATION_OPT = +native
else
	ERLANG_COMPILER_NATIVE_COMPILATION_OPT =
endif








# Erlang environment section.

ERLANG_USUAL_ROOT := ~/Software/Erlang/Erlang-current-install

ERLANG_COMPILER := `PATH=$(ERLANG_USUAL_ROOT)/bin:$$PATH which erlc`

ERLANG_BIN_ROOT := $(shell dirname $(ERLANG_COMPILER) )

ERLANG_ROOT := $(shell dirname $(ERLANG_BIN_ROOT) )


ERLANG_PLT_FILE := $(ERLANG_ROOT)/Erlang.plt


WARNING_LEVEL := 2

# Used even if not in debug mode:
ERLANG_COMPILER_OPT_DEBUG := +debug_info


# The number of available cores on this computer:
CORE_COUNT := `grep -c processor /proc/cpuinfo`


ERLANG_INTERPRETER := `PATH=$(ERLANG_USUAL_ROOT)/bin:$$PATH which erl`

# Not using '-smp auto' anymore, as the SMP mode is needed even with a single
# core if GUI (WxWindows) is to be used:
#
ERLANG_INTERPRETER_OPT_DEBUG := -init_debug -smp


# Finally not useful:
#ifndef FQDN

#FQDN    := `host \`hostname\` | awk '{ print $$1 }' | head -n 1`
FQDN    := `hostname -f`

#FQDN_OPT= "--fqdn $(FQDN)"

#endif


ifndef VM_TEST_NAME

VM_NAME := ceylan_debug

endif


ifndef VM_TEST_NAME

VM_TEST_NAME := ceylan_test

endif


#DISTRIBUTED_NODE_OPT := -sname $(VM_NAME) -setcookie $(PROJECT_NAME)
DISTRIBUTED_NODE_OPT := -name $(VM_NAME)@$(FQDN) -setcookie $(PROJECT_NAME)


STARTUP_FUNCTION := run

BEAM_DIRS += .                              \
			 $(COMMON_BEAM)/data-management \
			 $(COMMON_BEAM)/maths           \
			 $(COMMON_BEAM)/user-interface  \
			 $(COMMON_BEAM)/utils



ERL_LAUNCHER = $(COMMON_SRC)/scripts/launch-erl.sh



# Compiler section.


# Base compilation options that should be used for all modules (third-party
# included):
# -v/+verbose causes inlining to be too verbose, ex:
# "Old inliner: threshold=0 functions=[{setAttribute,3},.."
ERLANG_COMPILER_BASE_OPT += -b beam                                \
						  +\"{debug_info_key,\\\"Ceylan-common\\\"}\"


# Warning-related options (may or may not be included):
# "+{warn_format,2}" would trigger unwanted useless warnings about format
# strings
ERLANG_COMPILER_WARNING_OPT += -W$(WARNING_LEVEL) +warn_export_all  \
	+warn_export_vars +warn_shadow_vars +warn_obsolete_guards       \
	+warn_unused_import +warnings_as_errors

# Once type specs will have been added: +warn_untyped_record +warn_missing_spec


# No general inlining is enforced, as:
#
# - the induced gain is not obvious, compared to the non-inlined version
#
# - inlining seems to be triggered only through a in-file directive, whereas no
# general include is used here
#
# Otherwise we would have added, on a per-file basis:
# -compile(inline).
# -compile( {inline_size,64} ).



# Options for smart exceptions:

# Two versions of this package are available, we do not use the so-called
# 'stable' one, as it induced warnings (thus errors) at compilation-time (ex:
# hashtable.erl: Warning: variable 'KeyIndex' exported from 'case').
# The original code was correct, but the parse-transformed one not.
#SMART_EXCEPTION_PATH := $(COMMON_CONTRIB)/smart-exceptions/stable
SMART_EXCEPTION_PATH := $(COMMON_CONTRIB)/smart-exceptions/devel/src


# Note: activating smart exceptions may induce a (small) performance penalty:
# (currently they are disabled, as explained in the 'The display/1 Gotcha'
# thread of the erlang-question mailing list)
# ERLANG_SMART_EXCEPTION_OPT += +'{parse_transform,smart_exceptions}' \
#  -pa $(SMART_EXCEPTION_PATH)


ERLANG_COMPILER_OPT += $(ERLANG_COMPILER_BASE_OPT) $(INC)           \
	$(ERLANG_COMPILER_EXEC_TARGET_OPT) $(ERLANG_COMPILER_DEBUG_OPT) \
	$(ERLANG_COMPILER_NATIVE_COMPILATION_OPT)                       \
	$(ERLANG_COMPILER_WARNING_OPT) $(ERLANG_SMART_EXCEPTION_OPT)    \
	$(ERLANG_COMPILER_OPT_DEBUG) +bin_opt_info



# +v (verbose) option removed, as only active if the interpreter has been
# compiled in debug mode (otherwise a warning is displayed at each execution).
# (note: variable generally not used, as using the ERL_LAUNCHER script instead)
ERLANG_INTERPRETER_OPT := -pz $(BEAM_DIRS) $(DISTRIBUTED_NODE_OPT)


# We can force the launched VM to use TCP/IP connections in a range of ports
# enabled by the firewalls and/or to rely on a non-standard EPMD port:
#

# (note that, should the user code launch other VMs and/or have to communicate
# with others VMs, the corresponding settings will have to match these ones -
# think to all upper layers which may rely implicitly on that port information)
#
#FIREWALL_OPT :=
FIREWALL_OPT := --tcp-range 50000 55000 --epmd-port 4506


ifdef MAX_PROCESS_COUNT
	MAX_PROCESS_OPT := --max-process-count $(MAX_PROCESS_COUNT)
endif


ifdef ASYNC_THREAD_COUNT
	ASYNC_THREAD_OPT := --async-thread-count $(ASYNC_THREAD_COUNT)
endif





# 'BEAM_PATH_OPT = --beam-paths $(BEAM_DIRS)' would be shorter, but would
# prevent adding other options to the launcher afterwards:
# (now we use absolute directories - thanks to 'pwd' - instead of relative ones,
# as, otherwise, as soon as a process changes the current working directory, any
# further loading of a user-defined module, for example if used then for the
# first time, would fail).
BEAM_PATH_OPT = $(foreach dir,$(BEAM_DIRS),--beam-dir `pwd`/$(dir))


# Increases the distribution buffer busy limit (default is 1024):
# See http://erlang.org/doc/man/erl.html#+zdbbl.
# 1024*1024 = 1048576
BUSY_LIMIT_OPT := --busy-limit 1048576

SCALE_OPT := $(BUSY_LIMIT_OPT)


# Removed:
#
#  --ln $(VM_TEST_NAME)
#
#  -c $(PROJECT_NAME), as not specifying it on the command line allows to
# select the one in ~/.erlang.cookie (if any), which is better
#
#  -v, as the verbose mode for the launcher led to too many lines being output,
# now that user-defined code paths are absolute, thus significantly longer
#
# OVERRIDDEN_OPT left for the user.
ERL_PARAMETERIZED_LAUNCHER = $(ERL_LAUNCHER) $(FQDN_OPT) $(FIREWALL_OPT) \
							 $(MAX_PROCESS_OPT) $(ASYNC_THREAD_OPT)      \
							 $(BEAM_PATH_OPT) $(SCALE_OPT)               \
							 $(OVERRIDDEN_OPT)


# Generally the short names are preferred (which now is the default setting), as
# they resist to a non-existing or, more probably, improperly configured, DNS
# systems.
# Can also be overridden from the command-line (variable used in generic rules):
# (use net_utils:get_node_naming_mode/0 to detect the naming mode at runtime)
#NODE_NAMING="--ln"
NODE_NAMING="--sn"

INTERNAL_OPTIONS := -start-verbatim-options $(INTERNAL_OPTIONS)


DIALYZER := `PATH=$(ERLANG_USUAL_ROOT)/bin:$$PATH which dialyzer`

# Options for Dialyzer, see http://www.erlang.org/doc/man/dialyzer.html:
# Could be added: -Wunderspecs (obsolete: -Wbehaviours)
DIALYZER_OPT := -Wrace_conditions


# To use a generic rule to produce it (unless already defined):
ifndef PLT_FILE
		PLT_FILE = $(PACKAGE_TOP)/$(PACKAGE_NAME).plt
endif

# To use a generic rule to produce it (unless already defined):
ifndef TYPE_LIST_FILE
	   TYPE_LIST_FILE = $(PACKAGE_TOP)/declared-types-in-$(PACKAGE_NAME).txt
endif


# For later reuse in the next layer of the software stack:
COMMON_PLT_FILE := $(COMMON_TOP)/common.plt

# This is the merged PLT of the level just below in the software stack.
# For 'Common', it is the PLT of Erlang/OTP:
ifndef PREDECESSOR_PLT
  PREDECESSOR_PLT := $(ERLANG_PLT_FILE)
endif
