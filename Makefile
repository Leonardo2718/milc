OUTPUT_DIR ?= make_build
SOURCE_DIR = src

ALEX_SOURCE = MLexer.x
HS_SOURCES = Main.hs

ALEX_SOURCE_PATH = $(addprefix $(SOURCE_DIR)/,$(ALEX_SOURCE))
HS_SOURCES_PATH = $(addprefix $(SOURCE_DIR)/,$(HS_SOURCES))
ALEX_OUTPUT = $(addprefix $(OUTPUT_DIR)/,$(ALEX_SOURCE:.x=.hs))

MCOMP = $(OUTPUT_DIR)/mcomp

_dummy := $(shell mkdir -p $(OUTPUT_DIR))

.PHONY: all clean

all: $(MCOMP)

$(MCOMP): $(HS_SOURCES_PATH) $(ALEX_OUTPUT)
	ghc -o $@ -odir $(OUTPUT_DIR)/ -hidir $(OUTPUT_DIR)/ $(HS_SOURCES_PATH) $(ALEX_OUTPUT)

$(ALEX_OUTPUT): $(ALEX_SOURCE_PATH)
	alex $(ALEX_SOURCE_PATH) -o $@

clean:
	rm -rf $(OUTPUT_DIR)/*
