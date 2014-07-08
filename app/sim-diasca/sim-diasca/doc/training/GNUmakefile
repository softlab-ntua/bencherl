SIM_DIASCA_TOP = ../..


MODULES_DIRS = correction


TRAINING_PREFIX = SimDiasca-training

OVERALL_DOCUMENT_SOURCE = $(TRAINING_PREFIX).rst

TRAINING_DIR = $(TRAINING_PREFIX)

TRAINING_ARCHIVE = $(TRAINING_PREFIX)-elements

TRAINING_ARCHIVE_FILENAME = $(TRAINING_ARCHIVE).tar.bz2


info-doc:
	@echo "  Use 'make full-doc' to generate the Sim-Diasca Training material (in english)."


generate-training-archive: clean full-doc
	@mkdir $(TRAINING_DIR)
	@cp tmp-rst/*.pdf $(TRAINING_DIR)
	@cp -r exercises $(TRAINING_DIR)
	-@cd $(TRAINING_DIR) && \
	find . -name '.svn' -exec /bin/rm -rf '{}' ';' 2>/dev/null
	@tar cvjf $(TRAINING_ARCHIVE_FILENAME) $(TRAINING_PREFIX)
	@echo ; echo "$(TRAINING_ARCHIVE_FILENAME) is ready!"


clean: clean-training

clean-training:
	-@/bin/rm -rf $(TRAINING_DIR) $(TRAINING_ARCHIVE_FILENAME)


DOCUTILS_TOP = ..

include $(SIM_DIASCA_TOP)/GNUmakesettings.inc
