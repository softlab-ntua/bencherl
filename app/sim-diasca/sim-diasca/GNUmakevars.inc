# Prerequisite: SIM_DIASCA_TOP must be set.

# Project section.

# PROJECT_NAME should be defined on a per-project basis.
ifndef PROJECT_NAME
	   PROJECT_NAME = Sim-Diasca
endif


#SIM_DIASCA_VERSION := 2.1.2-beta-`date '+%Y%m%d-%Hh%M'`
#SIM_DIASCA_VERSION := 2.2.2-for-RELEASE
#SIM_DIASCA_VERSION := 2.2.3-for-T1-and-T2
#SIM_DIASCA_VERSION := 2.2.3-for-esl-work-t3
SIM_DIASCA_VERSION := 2.2.4


# PROJECT_VERSION should be defined on a per-project basis.
ifndef PROJECT_VERSION
	   PROJECT_VERSION = $(SIM_DIASCA_VERSION)
endif

ifndef PROJECT_RELEASE_DATE
	   # To be set only at release time:
	   #PROJECT_RELEASE_DATE = "(not set yet)"
	   PROJECT_RELEASE_DATE = "Thursday, April 7, 2011"
endif


ifndef PACKAGE_NAME
	   PACKAGE_NAME = sim-diasca
endif


ifndef PACKAGE_TOP
	   PACKAGE_TOP = $(SIM_DIASCA_TOP)
endif



# Better defined here, as depends on the Sim-Diasca version number:
SIM_DIASCA_RELEASE_BASENAME := Sim-Diasca-$(SIM_DIASCA_VERSION)
SIM_DIASCA_RELEASE_BASE = $(ROOT_TOP)/../$(SIM_DIASCA_RELEASE_BASENAME)

SIM_DIASCA_RELEASE_ARCHIVE_ZIP := $(SIM_DIASCA_RELEASE_BASENAME).zip
SIM_DIASCA_RELEASE_ARCHIVE_BZ2 := $(SIM_DIASCA_RELEASE_BASENAME).tar.bz2
SIM_DIASCA_RELEASE_ARCHIVE_XZ  := $(SIM_DIASCA_RELEASE_BASENAME).tar.xz

SIM_DIASCA_RELEASES := $(SIM_DIASCA_RELEASE_ARCHIVE_ZIP) \
					   $(SIM_DIASCA_RELEASE_ARCHIVE_BZ2) \
					   $(SIM_DIASCA_RELEASE_ARCHIVE_XZ)


ifndef VM_NAME

VM_NAME := sim_diasca_debug

endif


ifndef VM_TEST_NAME

VM_TEST_NAME := sim_diasca_test

endif



WORK_ARCHIVE_FILE  := \
  $(ARCHIVE_ROOT)/`date +'%Y%m%d'`-$(PROJECT_NAME)-work-archive.tar.xz




# Directory section.

VERSION_DIR = $(SIM_DIASCA_TOP)

FRAMEWORK_DIR = $(SIM_DIASCA_TOP)/src

CORE_BASE_DIR := $(FRAMEWORK_DIR)/core

CORE_SRC_DIR := $(CORE_BASE_DIR)/src

DATA_MANAGEMENT_BASE_DIR := $(CORE_SRC_DIR)/data-management
DEPLOYMENT_BASE_DIR      := $(CORE_SRC_DIR)/deployment
PLUGINS_BASE_DIR         := $(CORE_SRC_DIR)/plugins
RANDOM_BASE_DIR          := $(CORE_SRC_DIR)/random
RESILIENCE_BASE_DIR      := $(CORE_SRC_DIR)/resilience
SCENARIO_BASE_DIR        := $(CORE_SRC_DIR)/scenario
SCHEDULING_BASE_DIR      := $(CORE_SRC_DIR)/scheduling


DATA_EXCHANGE_BASE_DIR := $(DATA_MANAGEMENT_BASE_DIR)/data-exchange
DATA_STORAGE_BASE_DIR  := $(DATA_MANAGEMENT_BASE_DIR)/data-storage
MONITORING_BASE_DIR    := $(DATA_MANAGEMENT_BASE_DIR)/monitoring
RESULT_BASE_DIR        := $(DATA_MANAGEMENT_BASE_DIR)/result-management

PERFORMANCE_BASE_DIR := $(MONITORING_BASE_DIR)/performance
PROBE_BASE_DIR       := $(MONITORING_BASE_DIR)/probe

GENERIC_BASE_DIR := $(CORE_BASE_DIR)/generic
MESH_BASE_DIR    := $(CORE_BASE_DIR)/mesh
SPACE_BASE_DIR   := $(CORE_BASE_DIR)/space


MODELS_DIR = $(FRAMEWORK_DIR)/models

ELECTRICITY_BASE_DIR := $(MODELS_DIR)/electricity
EQUIPMENT_BASE_DIR   := $(MODELS_DIR)/equipment
METERING_BASE_DIR    := $(MODELS_DIR)/metering
RELIABILITY_BASE_DIR := $(MODELS_DIR)/reliability
TELECOM_BASE_DIR     := $(MODELS_DIR)/telecom

LOW_VOLTAGE_MESH_DIR  := $(ELECTRICITY_BASE_DIR)/low-voltage-mesh

METER_BASE_DIR        := $(METERING_BASE_DIR)/meter
CONCENTRATOR_BASE_DIR := $(METERING_BASE_DIR)/concentrator

FAILURE_BASE_DIR     := $(RELIABILITY_BASE_DIR)/failure
REPARATION_BASE_DIR  := $(RELIABILITY_BASE_DIR)/reparation

NETWORK_BASE_DIR                  := $(TELECOM_BASE_DIR)/network
INTERFERENCES_BASE_DIR            := $(TELECOM_BASE_DIR)/interferences
NETWORK_BASE_DIR                  := $(TELECOM_BASE_DIR)/network
POINT_TO_POINT_BASE_DIR           := $(TELECOM_BASE_DIR)/point-to-point
POWER_LINE_COMMUNICATION_BASE_DIR := \
								$(TELECOM_BASE_DIR)/power-line-communication
WIRELESS_BASE_DIR                 := $(TELECOM_BASE_DIR)/wireless



# Source section.

CORE_SRC := $(CORE_BASE_DIR)

DATA_EXCHANGER_SRC      := $(DATA_EXCHANGE_BASE_DIR)
DATA_LOGGER_SRC         := $(DATA_STORAGE_BASE_DIR)

PERFORMANCE_TRACKER_SRC := $(PERFORMANCE_BASE_DIR)
PROBE_SRC               := $(PROBE_BASE_DIR)
RESULT_SRC              := $(RESULT_BASE_DIR)


DEPLOYMENT_SRC := $(DEPLOYMENT_BASE_DIR)
PLUGINS_SRC    := $(PLUGINS_BASE_DIR)
RANDOM_SRC     := $(RANDOM_BASE_DIR)
RESILIENCE_SRC := $(RESILIENCE_BASE_DIR)
SCENARIO_SRC   := $(SCENARIO_BASE_DIR)
SCHEDULING_SRC := $(SCHEDULING_BASE_DIR)


GENERIC_SRC := $(GENERIC_BASE_DIR)/src
MESH_SRC    := $(MESH_BASE_DIR)/src
SPACE_SRC   := $(SPACE_BASE_DIR)/src


ELECTRICITY_SRC := $(ELECTRICITY_BASE_DIR)/src
EQUIPMENT_SRC   := $(EQUIPMENT_BASE_DIR)/src
METERING_SRC    := $(METERING_BASE_DIR)/src
RELIABILITY_SRC := $(RELIABILITY_BASE_DIR)/src
TELECOM_SRC     := $(TELECOM_BASE_DIR)/src

LOW_VOLTAGE_MESH_SRC := $(LOW_VOLTAGE_MESH_DIR)/src

METER_SRC        := $(METER_BASE_DIR)/src
CONCENTRATOR_SRC := $(CONCENTRATOR_BASE_DIR)/src

FAILURE_SRC     := $(FAILURE_BASE_DIR)/src
REPARATION_SRC  := $(REPARATION_BASE_DIR)/src

INTERFERENCES_SRC            := $(INTERFERENCES_BASE_DIR)/src
NETWORK_SRC                  := $(NETWORK_BASE_DIR)/src
POINT_TO_POINT_SRC           := $(POINT_TO_POINT_BASE_DIR)/src
POWER_LINE_COMMUNICATION_SRC := $(POWER_LINE_COMMUNICATION_BASE_DIR)/src
WIRELESS_SRC                 := $(WIRELESS_BASE_DIR)/src



# Include path section.


VERSION_INC             = -I$(VERSION_DIR)

CORE_INC                = -I$(CORE_SRC)
DATA_EXCHANGE_INC       = -I$(DATA_EXCHANGER_SRC)
DATA_STORAGE_INC        = -I$(DATA_LOGGER_SRC)
DEPLOYMENT_INC          = -I$(DEPLOYMENT_SRC)
PERFORMANCE_TRACKER_INC = -I$(PERFORMANCE_TRACKER_SRC)
PLUGINS_INC             = -I$(PLUGINS_SRC)
PROBE_INC               = -I$(PROBE_SRC)
RANDOM_INC              = -I$(RANDOM_SRC)
RESILIENCE_INC          = -I$(RESILIENCE_SRC)
RESULT_INC              = -I$(RESULT_SRC)
SCENARIO_INC            = -I$(SCENARIO_SRC)
SCHEDULING_INC          = -I$(SCHEDULING_SRC)


GENERIC_INC = -I$(GENERIC_SRC)
MESH_INC    = -I$(MESH_SRC)
SPACE_INC   = -I$(SPACE_SRC)

ELECTRICITY_INC = -I$(ELECTRICITY_SRC)
EQUIPMENT_INC   = -I$(EQUIPMENT_SRC)
METERING_INC    = -I$(METERING_SRC)
TELECOM_INC     = -I$(TELECOM_SRC)

LOW_VOLTAGE_MESH_INC = -I$(LOW_VOLTAGE_MESH_SRC)

METER_INC        = -I$(METER_SRC)
CONCENTRATOR_INC = -I$(CONCENTRATOR_SRC)

FAILURE_INC     = -I$(FAILURE_SRC)
REPARATION_INC  = -I$(REPARATION_SRC)

INTERFERENCES_INC            = -I$(INTERFERENCES_SRC)
NETWORK_INC                  = -I$(NETWORK_SRC)
POINT_TO_POINT_INC           = -I$(POINT_TO_POINT_SRC)
POWER_LINE_COMMUNICATION_INC = -I$(POWER_LINE_COMMUNICATION_SRC)
WIRELESS_INC                 = -I$(WIRELESS_SRC)



INC += $(VERSION_INC) $(CORE_INC)                                            \
	$(DATA_EXCHANGE_INC) $(DATA_STORAGE_INC) $(DEPLOYMENT_INC)               \
	$(PERFORMANCE_TRACKER_INC)                                               \
	$(PROBE_INC) $(PLUGIN_INC) $(RESULT_INC) $(RANDOM_INC) $(SCENARIO_INC)   \
	$(SCHEDULING_INC) $(RESILIENCE_INC)                                      \
	$(GENERIC_INC) $(MESH_INC) $(SPACE_INC)                                  \
	$(ELECTRICITY_INC) $(EQUIPMENT_INC) $(METERING_INC)                      \
	$(SCENARIO_INC) $(TELECOM_INC)                                           \
	$(LOW_VOLTAGE_MESH_INC)                                                  \
	$(METER_INC) $(CONCENTRATOR_INC)                                         \
	$(FAILURE_INC) $(REPARATION_INC)                                         \
	$(INTERFERENCES_INC) $(NETWORK_INC) $(POINT_TO_POINT_INC)                \
	$(POWER_LINE_COMMUNICATION_INC) $(WIRELESS_INC)



# BEAM path section.

CORE_BEAM := $(CORE_BASE_DIR)/src

DATA_EXCHANGE_BEAM       := $(DATA_EXCHANGE_BASE_DIR)
DATA_STORAGE_BEAM        := $(DATA_STORAGE_BASE_DIR)
DEPLOYMENT_BEAM          := $(DEPLOYMENT_BASE_DIR)
PERFORMANCE_TRACKER_BEAM := $(PERFORMANCE_BASE_DIR)
PROBE_BEAM               := $(PROBE_BASE_DIR)
PLUGINS_BEAM             := $(PLUGINS_BASE_DIR)
RANDOM_BEAM              := $(RANDOM_BASE_DIR)
RESILIENCE_BEAM          := $(RESILIENCE_BASE_DIR)
RESULT_BEAM              := $(RESULT_BASE_DIR)
SCENARIO_BEAM            := $(SCENARIO_BASE_DIR)
SCHEDULING_BEAM          := $(SCHEDULING_BASE_DIR)


GENERIC_BEAM := $(GENERIC_BASE_DIR)/src
MESH_BEAM    := $(MESH_BASE_DIR)/src
SPACE_BEAM   := $(SPACE_BASE_DIR)/src

ELECTRICITY_BEAM := $(ELECTRICITY_BASE_DIR)/src
EQUIPMENT_BEAM   := $(EQUIPMENT_BASE_DIR)/src
METERING_BEAM    := $(METERING_BASE_DIR)/src
TELECOM_BEAM     := $(TELECOM_BASE_DIR)/src

LOW_VOLTAGE_MESH_BEAM := $(LOW_VOLTAGE_MESH_DIR)/src

METER_BEAM        := $(METER_BASE_DIR)/src
CONCENTRATOR_BEAM := $(CONCENTRATOR_BASE_DIR)/src

FAILURE_BEAM     := $(FAILURE_BASE_DIR)/src
REPARATION_BEAM  := $(REPARATION_BASE_DIR)/src

INTERFERENCES_BEAM            := $(INTERFERENCES_BASE_DIR)/src
NETWORK_BEAM                  := $(NETWORK_BASE_DIR)/src
POINT_TO_POINT_BEAM           := $(POINT_TO_POINT_BASE_DIR)/src
POWER_LINE_COMMUNICATION_BEAM := $(POWER_LINE_COMMUNICATION_BASE_DIR)/src
WIRELESS_BEAM                 := $(WIRELESS_BASE_DIR)/src


# Used to be 'BEAM_DIRS := ... $(BEAM_DIRS)', but it led to multiple duplicates
# (multiple inclusions?)
BEAM_DIRS += $(CORE_BEAM)                                                     \
	$(DATA_EXCHANGE_BEAM) $(DATA_STORAGE_BEAM) $(DEPLOYMENT_BEAM)             \
	$(PERFORMANCE_TRACKER_BEAM) $(PROBE_BEAM) $(PLUGINS_BEAM)                 \
	$(RESULT_BEAM) $(RANDOM_BEAM) $(SCENARIO_BEAM)                            \
	$(SCHEDULING_BEAM) $(RESILIENCE_BEAM) $(GENERIC_BEAM) $(MESH_BEAM)        \
	$(SPACE_BEAM) $(ELECTRICITY_BEAM) $(EQUIPMENT_BEAM) $(METERING_BEAM)      \
	$(TELECOM_BEAM) $(LOW_VOLTAGE_MESH_BEAM)                                  \
	$(METER_BEAM) $(CONCENTRATOR_BEAM)                                        \
	$(FAILURE_BEAM) $(REPARATION_BEAM)                                        \
	$(INTERFERENCES_BEAM) $(NETWORK_BEAM) $(POINT_TO_POINT_BEAM)              \
	$(POWER_LINE_COMMUNICATION_BEAM) $(WIRELESS_BEAM)


# To compile a module (ex: a plugin) using a behaviour (ex: sim_diasca_plugin),
# the BEAM of that behaviour must be found:
#
ERLANG_COMPILER_OPT += -pa $(PLUGINS_BEAM)


# Specifying the root directory of Sim-Diasca allows to be able to find
# afterwards files (like the node cleaner script) regardless from where the
# interpreter is run.
INTERNAL_OPTIONS := --sim-diasca-root $(SIM_DIASCA_TOP)     \
				--sim-diasca-version $(SIM_DIASCA_VERSION)  \



# Must be available from the user's path:
LOGMX = `which logmx.sh`

PROJECT_CSS = $(SIM_DIASCA_TOP)/tools/css/SimDiasca-docutils.css


# For later reuse in the next layer of the software stack:
SIM_DIASCA_PLT_FILE := $(SIM_DIASCA_TOP)/sim-diasca.plt


# The configuration file to list the potential distributed hosts:
SIM_DIASCA_HOST_FILE := "sim-diasca-host-candidates.txt"



# Documentation section.

TECH_MANUAL := SimDiasca-technical-manual-english.pdf
TECH_MANUAL_DIR := technical-manual


DEV_GUIDE := SimDiasca-developer-guide-english.pdf
DEV_GUIDE_DIR := developer-guide


# This is the merged PLT of the level just below in the software stack.
# For Sim-Diasca, it is the PLT of 'Traces':
ifndef PREDECESSOR_PLT
   PREDECESSOR_PLT = $(TRACES_PLT_FILE)
endif


# TRACES_TOP is expected to be a symbolic link pointing to the actual Traces
# package to be used.
#
# This link should be created by the 'links' target of the root makefile, so
# that a layer is able to find the layer just above it without being itself
# modified.
#
TRACES_TOP = $(SIM_DIASCA_TOP)/../traces
