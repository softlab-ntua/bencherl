SIM_DIASCA_TOP = ../../../../../..


include $(SIM_DIASCA_TOP)/GNUmakesettings.inc


Test_probe.dat:
	@$(MAKE) class_Probe_run


test-plot-generation:
	@$(MAKE) && $(ERLANG_INTERPRETER) -run generate_plots_for_time_series run .
