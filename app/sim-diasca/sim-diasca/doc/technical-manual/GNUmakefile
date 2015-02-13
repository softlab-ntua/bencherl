SIM_DIASCA_TOP = ../..


.PHONY: all

OVERALL_DOCUMENT_SOURCE = $(TECH_MANUAL)


# Default do-nothing target:
all:
	@echo "  Run 'make full-doc' to generate the technical "\
	"manual ($(TECH_MANUAL))."


full-doc: full-doc-local


# We need to rely on images generated in other layers (ex: mock-simulators):
#
# (soda-ordering-interacting-FSM.png is a file generated in mock-simulators, yet
# we need it; we do not put the symlink in GIT as it would be dead as long as
# the PNG is not generated, and this would block the release preparation which
# attempts to follow symlinks; hence we recreate it iff needed)
#
full-doc-local:
	-@ln -s $(SIM_DIASCA_TOP)/../mock-simulators/soda-test/doc/soda-ordering-interacting-FSM.png
	@cd $(SIM_DIASCA_TOP)/.. && $(MAKE) -s doc



clean: clean-local

clean-local:
	-@/bin/rm -f soda-ordering-interacting-FSM.png


DOCUTILS_TOP = $(SIM_DIASCA_TOP)


include $(SIM_DIASCA_TOP)/GNUmakesettings.inc
