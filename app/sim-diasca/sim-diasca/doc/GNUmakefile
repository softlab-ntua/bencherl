SIM_DIASCA_TOP = ..


.PHONY: all release-doc doc-package technical-manual developer-guide info-doc \
		clean clean-doc


MODULES_DIRS = developer-guide technical-manual training

DOC_PACKAGE_PREFIX  = Sim-Diasca-$(SIM_DIASCA_VERSION)-doc


DOC_PACKAGE_ARCHIVE_ZIP = $(DOC_PACKAGE_PREFIX).zip
DOC_PACKAGE_ARCHIVE_BZ2 = $(DOC_PACKAGE_PREFIX).tar.bz2
DOC_PACKAGE_ARCHIVE_XZ  = $(DOC_PACKAGE_PREFIX).tar.xz


# Default do-nothing target:
all:
	@echo "  Run 'make doc-package' if wanting to generate a documentation \
	package, or 'make full-doc' to generate only the technical manual."


doc:


# Alternate named target:
release-doc: doc-package


# XZ target not activated, not usual enough:
doc-package: technical-manual developer-guide
	-@/bin/rm -rf $(DOC_PACKAGE_PREFIX)
	@/bin/mkdir $(DOC_PACKAGE_PREFIX)
	@/bin/cp $(TECH_MANUAL_DIR)/tmp-rst/$(TECH_MANUAL) $(DOC_PACKAGE_PREFIX)
	@/bin/cp $(DEV_GUIDE_DIR)/tmp-rst/$(DEV_GUIDE) $(DOC_PACKAGE_PREFIX)
	@tar cvjf $(DOC_PACKAGE_ARCHIVE_BZ2) $(DOC_PACKAGE_PREFIX)
	@zip -r $(DOC_PACKAGE_ARCHIVE_ZIP) $(DOC_PACKAGE_PREFIX)
	@echo "   $(DOC_PACKAGE_ARCHIVE_BZ2) and $(DOC_PACKAGE_ARCHIVE_ZIP) "\
	"generated in sim-diasca/doc"
	-@/bin/rm -rf $(DOC_PACKAGE_PREFIX)


technical-manual:
	@( cd $(TECH_MANUAL_DIR) && $(MAKE) -s full-doc )

developer-guide:
	@( cd $(DEV_GUIDE_DIR) && $(MAKE) -s full-doc )


info-doc:
	@echo "  Use 'make full-doc' to generate the Sim-Diasca Technical Manual"\
	" (in english)."
	@echo "DOC_PACKAGE_PREFIX = $(DOC_PACKAGE_PREFIX)"
	@echo "DOC_PACKAGE_ARCHIVE_ZIP = $(DOC_PACKAGE_ARCHIVE_ZIP)"
	@echo "DOC_PACKAGE_ARCHIVE_BZ2 = $(DOC_PACKAGE_ARCHIVE_BZ2)"
	@echo "DOC_PACKAGE_ARCHIVE_XZ  = $(DOC_PACKAGE_ARCHIVE_XZ)"


clean: clean-doc


# Not using DOC_PACKAGE_* as any version must be removed:
clean-doc:
	-@/bin/rm -f Sim-Diasca-*-doc.zip Sim-Diasca-*-doc.tar.bz2 \
		Sim-Diasca-*-doc.tar.xz
	-@/bin/rm -rf Sim-Diasca-*-doc


# Root specified as we want to catch the doc in src/core as well:
DOCUTILS_TOP = $(SIM_DIASCA_TOP)


include $(SIM_DIASCA_TOP)/GNUmakesettings.inc
