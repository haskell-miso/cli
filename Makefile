# Build the miso tool with an installed mhs, against the sources of
# MicroHs (MHSSRC) and MicroCabal (MCABALSRC).  The flake does the same.
MHS ?= mhs
MHSSRC ?= ../MicroHs
MCABALSRC ?= ../MicroCabal
CC ?= cc
CFLAGS ?= -O2
LDFLAGS ?=
RTS = $(MHSSRC)/src/runtime

.PHONY: all clean
all: build/miso

build/mcabal: scripts/prepare-microcabal.sh
	scripts/prepare-microcabal.sh $(MCABALSRC) build/mcabal

build/miso.c: build/mcabal src/Miso/CLI/*.hs
	$(MHS) -i$(MHSSRC)/src -i$(MHSSRC)/mhs -ibuild/mcabal -isrc -obuild/miso.c Miso.CLI.Main

build/miso: build/miso.c
	$(CC) $(CFLAGS) -w -I$(RTS) -I$(RTS)/unix $(RTS)/main.c $(RTS)/eval.c build/miso.c -lm $(LDFLAGS) -o build/miso

clean:
	rm -rf build .mhscache
