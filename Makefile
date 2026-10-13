# Build the miso tool with an installed mhs, against the sources of
# MicroHs (MHSSRC) and MicroCabal (MCABALSRC).  The flake does the same.
MHS ?= mhs
MHSSRC ?= ../MicroHs
MCABALSRC ?= ../MicroCabal
CC ?= cc
CFLAGS ?= -O2
LDFLAGS ?=
# the runtime configuration: unix, or mingw for Windows (with EXE=.exe)
CONF ?= unix
EXE ?=
RTS = $(MHSSRC)/src/runtime

.PHONY: all clean
all: build/miso$(EXE) build/cpphs$(EXE)

build/mcabal: scripts/prepare-microcabal.sh
	scripts/prepare-microcabal.sh $(MCABALSRC) build/mcabal

build/miso.c: build/mcabal src/Miso/CLI/*.hs
	$(MHS) -i$(MHSSRC)/src -i$(MHSSRC)/mhs -ibuild/mcabal -isrc -obuild/miso.c Miso.CLI.Main

build/miso$(EXE): build/miso.c
	$(CC) $(CFLAGS) -w -I$(RTS) -I$(RTS)/$(CONF) $(RTS)/main.c $(RTS)/eval.c build/miso.c -lm $(LDFLAGS) -o build/miso$(EXE)

# cpphs, which mhs runs on modules with CPP.  It is a separate program (it
# has its own main and combinators), shipped next to miso in bin/.
build/cpphs$(EXE): $(MHSSRC)/generated/cpphs.c
	@mkdir -p build
	$(CC) $(CFLAGS) -w -I$(RTS) -I$(RTS)/$(CONF) $(RTS)/main.c $(RTS)/eval.c $(MHSSRC)/generated/cpphs.c -lm $(LDFLAGS) -o build/cpphs$(EXE)

clean:
	rm -rf build .mhscache
