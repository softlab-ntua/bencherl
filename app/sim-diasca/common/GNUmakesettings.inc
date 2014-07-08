# This is the file that all 'Common' GNUmakefiles, and only them, should
# include.

# Prerequisite: COMMON_TOP must be set.


# Each project should define its own GNUmakesettings.inc, as explained below,
# and should not reuse any other GNUmakesettings.inc coming from prerequisites.


# Note regarding dependencies and settings:

# let c be 'common', b be a library using 'common' (ex: 'wooper') and a an
# application using the b library: a depends on b that depends on c.

# For any makefile of a, the correct include order would be:
#
#  - all GNUmakevars before all GNUmakerules
#
#  - among GNUmakevars, first a, then b, then c, as b will define variables if a
# did not defined them, and so on
#
# - among GNUmakerules, first a, then b, then c, for the same reason: rules will
# be matched according to their declaration order
#
# Therefore we need to have:
# a.GNUmakevars
# b.GNUmakevars
# c.GNUmakevars
# a.GNUmakerules
# b.GNUmakerules
# c.GNUmakerules

# Hence, for no project, the GNUmakevars and GNUmakerules should be aggregated.

# As it is a lot more convenient that all GNUmakefiles include only one global
# GNUmake*inc file, each project should define an appropriate one, which ought
# be named 'GNUmakesettings.inc' (this file).


include $(COMMON_TOP)/GNUmakevars.inc

include $(COMMON_TOP)/GNUmakerules.inc
