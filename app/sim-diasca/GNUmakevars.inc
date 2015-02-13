# This is the GNUmakevars.inc which is solely dedicated to the top GNUmakefile.


PROJECT_NAME := Sim-Diasca


SIM_DIASCA_TOP := $(ROOT_TOP)/sim-diasca


# A file that is generated so that all version numbers of interest are known
# of the engine.
#
VERSION_FILE := $(SIM_DIASCA_TOP)/package-versions.hrl


SIM_DIASCA_PACKAGE_ELEMENTS = common wooper traces sim-diasca mock-simulators \
	GNUmakefile GNUmakesettings.inc GNUmakevars.inc .gitignore


SERVER := claui2a4.der.edf.fr

ARCHIVE_SERVER := $(SERVER)
PRINT_SERVER   := $(SERVER)


#ARCHIVE_USER := `whoami`
ARCHIVE_USER := E21850
ARCHIVE_LOCATION := $(ARCHIVE_USER)@$(ARCHIVE_SERVER):/home/$(ARCHIVE_USER)/Archives/$(PROJECT_NAME)

PRINT_USER = `whoami`
PRINT_LOCATION := $(PRINT_USER)@$(PRINT_SERVER):/home/$(PRINT_USER)
