MOCK_SIMULATORS_TOP = ../..

.PHONY: all batch trace demo minimal clean clean-local


# Setting duration and scale:


# Licit durations are brief, short, medium and long:
CASE_DURATION := brief
#CASE_DURATION := short
#CASE_DURATION := medium
#CASE_DURATION := long


# Licit scales are: tiny, small, medium, large and huge:
CASE_SCALE := tiny
#CASE_SCALE := small
#CASE_SCALE := medium
#CASE_SCALE := large
#CASE_SCALE := huge


CASE_BASE_PARAMS := "--duration $(CASE_DURATION) --scale $(CASE_SCALE)"



# Setting how interactive the execution should be:

BATCH_PARAMS := "--batch $(CASE_BASE_PARAMS)"

TRACE_PARAMS := "$(CASE_BASE_PARAMS)"



# Setting whether we want to develop and troubleshoot, or go for higher scale in
# production:
#
# Note: must be specified also at *compilation-time* (in addition to run-time)
#
# Ex: to really run in production mode, enter from the root of the sources:
# 'make clean all EXECUTION_TARGET=production'
#

# We define a local settings here, not to interfere with the rest of the build:
#LOCAL_EXECUTION_TARGET := development
LOCAL_EXECUTION_TARGET := production




all:


batch:
	@$(MAKE) city_benchmarking_run CMD_LINE_OPT="--batch --duration $(CASE_DURATION) --scale $(CASE_SCALE)" EXECUTION_TARGET=$(LOCAL_EXECUTION_TARGET)


trace:
	@$(MAKE) city_benchmarking_run CMD_LINE_OPT="--duration $(CASE_DURATION) --scale $(CASE_SCALE)" EXECUTION_TARGET=$(LOCAL_EXECUTION_TARGET)


demo:
	@$(MAKE) city_benchmarking_run CMD_LINE_OPT="--duration short --scale tiny" EXECUTION_TARGET=development


minimal:
	@$(MAKE) city_benchmarking_run CMD_LINE_OPT="--duration brief --scale tiny --batch" EXECUTION_TARGET=development


clean: clean-local


clean-local:
	- @/bin/rm -f Road_Network_2D_Topology.graph


include $(MOCK_SIMULATORS_TOP)/GNUmakesettings.inc
