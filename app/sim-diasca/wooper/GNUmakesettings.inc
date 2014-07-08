# This is the file that all WOOPER GNUmakefiles, and only them, should include.

# Prerequisite: WOOPER_TOP must be set.


# Each project should define its own GNUmakesettings.inc, as explained in
# common/GNUmakesettings.inc.


# Nothing simpler can be used:

include $(WOOPER_TOP)/GNUmakevars.inc
include $(COMMON_TOP)/GNUmakevars.inc

include $(WOOPER_TOP)/GNUmakerules.inc
include $(COMMON_TOP)/GNUmakerules.inc
