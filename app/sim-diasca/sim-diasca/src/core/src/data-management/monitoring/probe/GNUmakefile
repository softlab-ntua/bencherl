SIM_DIASCA_TOP = ../../../../../..

.PHONY: test-plot-generation real-clean

include $(SIM_DIASCA_TOP)/GNUmakesettings.inc


Test_probe.dat:
	@$(MAKE) class_Probe_run


test-plot-generation:
	@$(MAKE) && $(ERLANG_INTERPRETER) -run generate_plots_for_time_series run .


real-clean: real-clean-local


real-clean-local:
	-@/bin/rm -rf Basic_probe_test-on-*
