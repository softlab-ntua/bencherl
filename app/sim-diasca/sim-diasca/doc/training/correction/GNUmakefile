SIM_DIASCA_TOP = ../../..



CORRECTION_PREFIX = SimDiasca-training-corrected-exercises

OVERALL_DOCUMENT_SOURCE = $(CORRECTION_PREFIX).rst

CORRECTION_DIR = $(CORRECTION_PREFIX)

CORRECTION_ARCHIVE = $(CORRECTION_PREFIX)

CORRECTION_ARCHIVE_FILENAME = $(CORRECTION_ARCHIVE).tar.bz2



info-doc:
	@echo "  Use 'make full-doc' to generate the corrections corresponding to the Sim-Diasca Training material (in english)."


generate-correction-archive: clean full-doc
	@mkdir $(CORRECTION_DIR)
	@cp tmp-rst/*.pdf $(CORRECTION_DIR)
	@cp -r ex_* $(CORRECTION_DIR)
	@tar cvjf $(CORRECTION_ARCHIVE_FILENAME) $(CORRECTION_PREFIX)
	@echo ; echo "$(CORRECTION_ARCHIVE_FILENAME) is ready!"


clean: clean-correction


clean-correction:
	-@/bin/rm -rf $(CORRECTION_DIR) $(CORRECTION_ARCHIVE_FILENAME)



DOCUTILS_TOP = ../..

include $(SIM_DIASCA_TOP)/GNUmakesettings.inc
