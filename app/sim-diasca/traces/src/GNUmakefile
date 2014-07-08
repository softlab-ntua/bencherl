TRACES_TOP = ..


# Must be the first target, otherwise BEAM files will not be built:
all:


include $(TRACES_TOP)/GNUmakesettings.inc



# Lists the modules that should be recompiled once TraceType (as defined in
# traces.hrl) is modified:
#
# (however all other modules including macros like 'test_start' should be
# rebuilt as well)
#
TRACES_TYPE_DEPENDENT_FILES = \
	class_TraceEmitter.beam    \
	class_TraceSupervisor.beam \
	class_TraceAggregator.beam


$(TRACES_TYPE_DEPENDENT_FILES): traces.hrl
