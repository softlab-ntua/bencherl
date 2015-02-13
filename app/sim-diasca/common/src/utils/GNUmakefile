COMMON_TOP = ../..

.PHONY: clean-local test-local

# So that it remains the default target:
all:

clean: clean-local


clean-local:
	-@/bin/rm -f my-content-file my-content-file.encrypted \
		my-content-file.decrypted my-test-key-file.cipher .cipher-*


test: test-local


test-local:
	@$(MAKE) most_basic_example_exec


include $(COMMON_TOP)/GNUmakesettings.inc
