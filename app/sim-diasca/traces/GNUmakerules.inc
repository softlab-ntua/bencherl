# Prerequisite: TRACES_TOP must be set.

# Add all Trace-specific rules below.



.PHONY: clean-traces


# Not wanting to define a default rule:
#clean: clean-traces


ERL_FILES = $(wildcard *.erl)

TRACES_FILES = $(patsubst %.erl,%.traces,$(ERL_FILES))


AUX_GENERATED_FILES  := $(AUX_GENERATED_FILES)  \
	$(patsubst %.traces,%.aux,$(TRACES_FILES))

TEX_GENERATED_FILES  := $(TEX_GENERATED_FILES)  \
	$(patsubst %.traces,%.tex,$(TRACES_FILES))

OUT_GENERATED_FILES  := $(OUT_GENERATED_FILES)  \
	$(patsubst %.traces,%.out,$(TRACES_FILES))

LOG_GENERATED_FILES  := $(LOG_GENERATED_FILES)  \
	$(patsubst %.traces,%.log,$(TRACES_FILES))

HTML_GENERATED_FILES := $(HTML_GENERATED_FILES) \
	$(patsubst %.traces,%.html,$(TRACES_FILES))

PDF_GENERATED_FILES  := $(PDF_GENERATED_FILES)  \
	$(patsubst %.traces,%.pdf,$(TRACES_FILES))


TRACES_GENERATED_FILES := $(AUX_GENERATED_FILES) $(TEX_GENERATED_FILES)    \
	$(OUT_GENERATED_FILES) $(LOG_GENERATED_FILES) $(HTML_GENERATED_FILES)  \
	$(PDF_GENERATED_FILES) $(GENERATOR_LOG_FILE) Listener-*.traces


# Used by the clean-override target in 'Common':
FILES_TO_CLEAN += $(TRACES_FILES) $(TRACES_GENERATED_FILES)
