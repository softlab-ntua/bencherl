APP_NAME:=webmachine
DEPS:=mochiweb-wrapper

UPSTREAM_HG:=http://bitbucket.org/justin/webmachine
UPSTREAM_REVISION:=0c4b60ac68b4
RETAIN_ORIGINAL_VERSION:=true

WRAPPER_PATCHES:=uneunit.patch 10-crypto.patch xref-fix.patch ssl.patch

ORIGINAL_APP_FILE=$(CLONE_DIR)/ebin/$(APP_NAME).app
DO_NOT_GENERATE_APP_FILE=true

# Webmachine source files do -include("include/...")
PACKAGE_ERLC_OPTS+=-I $(CLONE_DIR)

define package_rules

# This rule is run *before* the one in do_package.mk
$(PLUGINS_SRC_DIST_DIR)/$(PACKAGE_DIR)/.srcdist_done::
	cp $(CLONE_DIR)/LICENSE $(PACKAGE_DIR)/LICENSE-Apache-Basho

endef
