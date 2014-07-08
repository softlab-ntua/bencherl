# This is the file that all GNUmakefiles of the full distribution, and only
# them, should include.

# Prerequisite: ROOT_TOP must be set.


# Each project should define its own GNUmakesettings.inc, as explained in
# common/GNUmakesettings.inc.


# Nothing simpler can be used:


include $(MOCK_SIMULATORS_TOP)/GNUmakevars.inc
include $(SIM_DIASCA_TOP)/GNUmakevars.inc
include $(TRACES_TOP)/GNUmakevars.inc
include $(WOOPER_TOP)/GNUmakevars.inc
include $(COMMON_TOP)/GNUmakevars.inc

include $(ROOT_TOP)/GNUmakevars.inc

include $(MOCK_SIMULATORS_TOP)/GNUmakerules.inc
include $(SIM_DIASCA_TOP)/GNUmakerules.inc
include $(TRACES_TOP)/GNUmakerules.inc
include $(WOOPER_TOP)/GNUmakerules.inc
include $(COMMON_TOP)/GNUmakerules.inc
