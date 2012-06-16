APP_NAME:=mochiweb

UPSTREAM_GIT:=http://github.com/mochi/mochiweb.git
UPSTREAM_REVISION:=9a53dbd7b2c52eb5b9d4
RETAIN_ORIGINAL_VERSION:=true
WRAPPER_PATCHES:=mochiweb-12b3.patch 10-crypto.patch 20-MAX_RECV_BODY.patch

# internal.hrl is used by webmachine
UPSTREAM_INCLUDE_DIRS+=$(CLONE_DIR)/src

ORIGINAL_APP_FILE:=$(CLONE_DIR)/$(APP_NAME).app
DO_NOT_GENERATE_APP_FILE=true

define package_rules

$(CLONE_DIR)/src/$(APP_NAME).app.src: $(CLONE_DIR)/.done

$(ORIGINAL_APP_FILE): $(CLONE_DIR)/src/$(APP_NAME).app.src
	$(CLONE_DIR)/support/make_app.escript $$< $$@ "" "`cd $(CLONE_DIR)/src && echo *.erl | sed 's|\.erl||g'`"

$(PACKAGE_DIR)+clean::
	rm -rf $(ORIGINAL_APP_FILE)

# This rule is run *before* the one in do_package.mk
$(PLUGINS_SRC_DIST_DIR)/$(PACKAGE_DIR)/.srcdist_done::
	cp $(CLONE_DIR)/LICENSE $(PACKAGE_DIR)/LICENSE-MIT-Mochi

endef
