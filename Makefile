#
# Copyright (C) 2018 Leonardo Banderali
#
# License:
#
#     Permission is hereby granted, free of charge, to any person obtaining a copy
#     of this software and associated documentation files (the "Software"), to deal
#     in the Software without restriction, including without limitation the rights
#     to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
#     copies of the Software, and to permit persons to whom the Software is
#     furnished to do so, subject to the following conditions:
#
#     The above copyright notice and this permission notice shall be included in
#     all copies or substantial portions of the Software.
#
#     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#     IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#     FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#     AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#     LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#     OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
#     THE SOFTWARE.
#

OUTPUT_DIR ?= make_build
SOURCE_DIR = src

ALEX_SOURCE = MLexer.x
HS_SOURCES = \
	CompilerEnvironment.hs \
	MilcUtils.hs MilcAST.hs \
	MRDParser.hs \
	MIL.hs \
	MilcCFG.hs \
	MilcOptimizer.hs \
	MILGenerator.hs \
	MEncoder.hs \
	RSMGenerator.hs \
	Main.hs

ALEX_SOURCE_PATH = $(addprefix $(SOURCE_DIR)/,$(ALEX_SOURCE))
HS_SOURCES_PATH = $(addprefix $(SOURCE_DIR)/,$(HS_SOURCES))
ALEX_OUTPUT = $(addprefix $(OUTPUT_DIR)/,$(ALEX_SOURCE:.x=.hs))

MILC = $(OUTPUT_DIR)/milc

_dummy := $(shell mkdir -p $(OUTPUT_DIR))

.PHONY: all clean

all: $(MILC)

$(MILC): $(HS_SOURCES_PATH) $(ALEX_OUTPUT)
	ghc -o $@ -odir $(OUTPUT_DIR)/ -hidir $(OUTPUT_DIR)/ $(HS_SOURCES_PATH) $(ALEX_OUTPUT)

$(ALEX_OUTPUT): $(ALEX_SOURCE_PATH)
	alex $(ALEX_SOURCE_PATH) -o $@

clean:
	rm -rf $(OUTPUT_DIR)/*
