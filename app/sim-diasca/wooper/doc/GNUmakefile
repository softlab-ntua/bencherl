WOOPER_TOP = ..


OVERALL_DOCUMENT_TARGET = wooper.rst

DOC_GENERATED_FILES = wooper.pdf wooper.html

DOC_FILES = $(DOC_GENERATED_FILES) wooper-example.png


WOOPER_WEB_DIR = $(WOOPER_TOP)/../../../../doc/web/main/documentation/wooper



full-doc:
	@$(MAKE) $(DOC_GENERATED_FILES) VIEW_PDF=no


# Exports in Ceylan website:
export-doc: doc
	@echo "   Exporting WOOPER documentation to $(WOOPER_WEB_DIR)"
	@/bin/cp -f $(DOC_FILES) $(WOOPER_WEB_DIR)
	@/bin/cp -f $(WOOPER_WEB_DIR)/wooper.html $(WOOPER_WEB_DIR)/index.html


# For COMMON_TOP:
include $(WOOPER_TOP)/GNUmakesettings.inc

# Not used, for a better readability:
#PROJECT_CSS = $(COMMON_TOP)/doc/Ceylan-docutils.css

# Now included in the common general rules:
#include $(COMMON_TOP)/doc/GNUmakerules-docutils.inc
