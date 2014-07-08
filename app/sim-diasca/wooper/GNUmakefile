WOOPER_TOP = .


.PHONY: all register-version-in-header                               \
		send-release release release-zip release-bz2 release-xz      \
		prepare-release clean-release clean-archive


MODULES_DIRS = src doc examples


# To override the 'all' default target with a parallel version:
BASE_MAKEFILE = true


WOOPER_RELEASES = $(WOOPER_RELEASE_ARCHIVE_BZ2) \
				  $(WOOPER_RELEASE_ARCHIVE_ZIP) \
				  $(WOOPER_RELEASE_ARCHIVE_XZ)


SF_USER = wondersye


# First target for default:
all:


register-version-in-header:
	@echo "-define( wooper_version, \"$(WOOPER_VERSION)\" )." >> $(VERSION_FILE)


# Note: the source archives are not produced in this directory, but in its
# parent, so that everything related to WOOPER (including these rules) remains
# self-contained.

send-release: release
	@echo "     Sending WOOPER releases $(WOOPER_RELEASES) to Sourceforge"
	@cd .. && rsync -avP -e ssh $(WOOPER_RELEASES) $(SF_USER)@frs.sourceforge.net:uploads/


release: release-zip release-bz2 release-xz
	@$(MAKE) clean-release


release-zip: prepare-release
	@echo "     Creating WOOPER release archive $(WOOPER_RELEASE_ARCHIVE_ZIP)"
	@cd .. && zip -r $(WOOPER_RELEASE_ARCHIVE_ZIP) $(WOOPER_RELEASE_BASE) \
	&& echo "     Archive $(WOOPER_RELEASE_ARCHIVE_ZIP) ready in "`pwd`


release-bz2: prepare-release
	@echo "     Creating WOOPER release archive $(WOOPER_RELEASE_ARCHIVE_BZ2)"
	@cd .. && tar chvjf $(WOOPER_RELEASE_ARCHIVE_BZ2) $(WOOPER_RELEASE_BASE) \
	&& echo "     Archive $(WOOPER_RELEASE_ARCHIVE_BZ2) ready in "`pwd`


release-xz: prepare-release
	@echo "     Creating WOOPER release archive $(WOOPER_RELEASE_ARCHIVE_XZ)"
	@cd .. && tar chvjf $(WOOPER_RELEASE_ARCHIVE_XZ) $(WOOPER_RELEASE_BASE) \
	&& echo "     Archive $(WOOPER_RELEASE_ARCHIVE_XZ) ready in "`pwd`


# The '-L' option with cp is used so that symbolic links are replaced by their
# actual target file, otherwise tar would include dead links in releases.
prepare-release: clean clean-release
	@echo "     Preparing release archive for WOOPER $(WOOPER_VERSION)"
	@cd .. && mkdir -p $(WOOPER_RELEASE_BASE) && /bin/cp -L -r common wooper $(WOOPER_RELEASE_BASE)
	@cd ../$(WOOPER_RELEASE_BASE) && mv wooper/top-GNUmakefile-for-releases GNUmakefile
	-@cd .. && find $(WOOPER_RELEASE_BASE) -type d -a -name '.svn' -exec /bin/rm -rf '{}' ';' 2>/dev/null
	-@cd .. && find $(WOOPER_RELEASE_BASE) -type d -a -name '.git' -exec /bin/rm -rf '{}' ';' 2>/dev/null
	-@cd .. && find $(WOOPER_RELEASE_BASE) -type f -a -name '*.beam' -exec /bin/rm -f '{}' ';' 2>/dev/null


clean: clean-release clean-archive


clean-release:
	@echo "     Cleaning release archive for WOOPER"
	-@cd .. && /bin/rm -rf $(WOOPER_RELEASE_BASE)


clean-archive:
	-@cd .. && /bin/rm -f $(WOOPER_RELEASES)


include $(WOOPER_TOP)/GNUmakesettings.inc
