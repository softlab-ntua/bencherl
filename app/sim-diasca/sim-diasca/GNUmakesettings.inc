# This is the file that all Sim-Diasca GNUmakefiles, and only them, should
# include.

# Prerequisite: SIM_DIASCA_TOP must be set.


# Each project should define its own GNUmakesettings.inc, as explained in
# common/GNUmakesettings.inc.


# Nothing simpler can be used:
include $(SIM_DIASCA_TOP)/GNUmakevars.inc
include $(TRACES_TOP)/GNUmakevars.inc
include $(WOOPER_TOP)/GNUmakevars.inc
include $(COMMON_TOP)/GNUmakevars.inc

include $(SIM_DIASCA_TOP)/GNUmakerules.inc
include $(TRACES_TOP)/GNUmakerules.inc
include $(WOOPER_TOP)/GNUmakerules.inc
include $(COMMON_TOP)/GNUmakerules.inc
