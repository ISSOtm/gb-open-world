
.SUFFIXES:
# Plain `sh` is typically used in Makefiles, but I'm afraid I may be relying on Bash-only features.
# Plus, portability isn't a primary goal; thus, I'm enforcing Bash here.
# If you need `sh` for some reason, and there's a compatibility issue, you can open a ticket at
# https://github.com/ISSOtm/gb-open-world/issues
SHELL := /bin/bash

################################################
#                                              #
#             CONSTANT DEFINITIONS             #
#                                              #
################################################

# Directory constants
BINDIR := bin
OBJDIR := obj
DEPDIR := dep

# Program constants
ifneq ($(shell which rm),)
    # POSIX OSes
    RM_RF := rm -rf
    MKDIR_P := mkdir -p
    PY :=
    filesize = echo 'NB_PB$2_BLOCKS equ (' `wc -c $1 | cut -d ' ' -f 1` ' + $2 - 1) / $2'
else
    # Windows outside of a POSIX env (Cygwin, MSYS2, etc.)
    # We need Powershell to get any sort of decent functionality
    $(warning Powershell is required to get basic functionality)
    RM_RF := -del /q
    MKDIR_P := -mkdir
    PY := python
    filesize = powershell Write-Output $$('NB_PB$2_BLOCKS equ ' + [string] [int] (([IO.File]::ReadAllBytes('$1').Length + $2 - 1) / $2))
endif

SUPERFAMICONV := src/tools/superfamiconv/bin/superfamiconv

# Shortcut if you want to use a local copy of RGBDS
RGBDS   :=
RGBASM  := $(RGBDS)rgbasm
RGBLINK := $(RGBDS)rgblink
RGBFIX  := $(RGBDS)rgbfix
RGBGFX  := $(RGBDS)rgbgfx

ROM = $(BINDIR)/$(ROMNAME).$(ROMEXT)

# Argument constants
INCDIRS  = src/ src/include/
WARNINGS = all extra
ASFLAGS  = -p $(PADVALUE) $(addprefix -i,$(INCDIRS)) $(addprefix -W,$(WARNINGS))
LDFLAGS  = -p $(PADVALUE)
FIXFLAGS = -p $(PADVALUE) -v -i "$(GAMEID)" -k "$(LICENSEE)" -l $(OLDLIC) -m $(MBC) -n $(VERSION) -r $(SRAMSIZE) -t $(TITLE)

# The list of "root" ASM files that RGBASM will be invoked on
SRCS = $(wildcard src/*.asm)

## Project-specific configuration
# Use this to override the above
include project.mk

################################################
#                                              #
#                    TARGETS                   #
#                                              #
################################################

# `all` (Default target): build the ROM
all: $(ROM)
.PHONY: all

# `mostlyclean`: Clean temp and bin files
mostlyclean:
	$(RM_RF) $(BINDIR)
	$(RM_RF) $(OBJDIR)
	$(RM_RF) $(DEPDIR)
	$(RM_RF) res
.PHONY: mostlyclean

# `clean`: Clean absolutely everything
clean: mostlyclean
	+env -C src/tools/map_converter cargo clean
.PHONY: clean

# `rebuild`: Build everything from scratch
# It's important to do these two in order if we're using more than one job
rebuild:
	$(MAKE) clean
	$(MAKE) all
.PHONY: rebuild

################################################
#                                              #
#                GIT SUBMODULES                #
#                                              #
################################################

# By default, cloning the repo does not init submodules
# If that happens, warn the user
# Note that the real paths aren't used!
# Since RGBASM fails to find the files, it outputs the raw paths, not the actual ones.
hardware.inc/hardware.inc rgbds-structs/structs.asm:
	@echo 'hardware.inc is not present; have you initialized submodules?'
	@echo 'Run `git submodule update --init`, then `make clean`, then `make` again.'
	@echo 'Tip: to avoid this, use `git clone --recursive` next time!'
	@exit 1

################################################
#                                              #
#                RESOURCE FILES                #
#                                              #
################################################

# By default, asset recipes convert files in `res/` into other files in `res/`
# This line causes assets not found in `res/` to be also looked for in `src/res/`
# "Source" assets can thus be safely stored there without `make clean` removing them
VPATH := src

res/%.1bpp: res/%.png
	@$(MKDIR_P) $(@D)
	$(RGBGFX) -d 1 -o $@ $<


# Don't use the variable for this one, this is only to build the bundled one
# We don't want to break overriding, now would we?
src/tools/superfamiconv/bin/superfamiconv:
	make -C src/tools/superfamiconv bin/superfamiconv

res/%.pal res/%.pal.json: res/%.png $(SUPERFAMICONV)
	@$(MKDIR_P) $(@D)
	$(SUPERFAMICONV) palette -M gbc $(SFC_PAL_FLAGS) -i "$<" -d "res/$*.pal" -j "res/$*.pal.json"

res/%.2bpp: res/%.png res/%.pal.json $(SUPERFAMICONV)
	@$(MKDIR_P) $(@D)
	$(SUPERFAMICONV) tiles -M gbc -B 2 $(SFC_TILE_FLAGS) -i "$<" -p "res/$*.pal.json" -d "$@"

res/%.map res/%.palmap: res/%.png res/%.pal.json res/%.2bpp $(SUPERFAMICONV)
	@$(MKDIR_P) $(@D)
	$(SUPERFAMICONV) map -M gbc -B 2 $(SFC_MAP_FLAGS) -i "$<" -p "res/$*.pal.json" -t "res/$*.2bpp" -d "res/$*.map" --out-pal-map "res/$*.palmap"


# Define how to compress files using the PackBits16 codec
# Compressor script requires Python 3
res/%.pb16: res/% src/tools/pb16.py
	@$(MKDIR_P) $(@D)
	$(PY) src/tools/pb16.py $< res/$*.pb16

res/%.pb16.size: res/%
	@$(MKDIR_P) $(@D)
	$(call filesize,$<,16) > res/$*.pb16.size


# Define how to compress files using the PackBits8 codec
# Compressor script requires Python 3
res/%.pb8: res/% src/tools/pb8.py
	@$(MKDIR_P) $(@D)
	$(PY) src/tools/pb8.py $< res/$*.pb8

res/%.pb8.size: res/%
	@$(MKDIR_P) $(@D)
	$(call filesize,$<,8) > res/$*.pb8.size


res/%.vwf: src/gb-vwf/make_font.py res/%.png
	@$(MKDIR_P) $(@D)
	$(PY) $^ $@


res/charmap.asm $(DEPDIR)/charmap.mk: src/vwf.asm
	@$(MKDIR_P) res $(DEPDIR)
	$(RGBASM) $(ASFLAGS) -M $(DEPDIR)/charmap.mk -MG -MP -MQ res/charmap.asm -MQ $(DEPDIR)/charmap.mk $< > res/charmap.asm


MAP_LIST := $(patsubst src/res/maps/%/,%,$(wildcard src/res/maps/*/))

res/maps.asm: $(patsubst %,res/maps/%/map.asm,$(MAP_LIST))
	@$(MKDIR_P) $(@D)
	echo 'rsreset ; This file is automatically generated by the Makefile. Manual edits will be lost!' >$@
	for map in $(MAP_LIST); do cat >>$@ <<<"INCLUDE \"res/maps/$${map}/map.asm\""; done

MAP_CONVERTER := src/tools/map_converter/target/release/map_converter

$(MAP_CONVERTER): $(wildcard src/tools/map_converter/src/*.rs) src/tools/map_converter/Cargo.toml
	+env -C src/tools/map_converter cargo build --release

res/maps/%/map.asm: res/maps/%/tileset.png $(MAP_CONVERTER) src/res/maps/%/map.tmx res/maps/%/tileset.pal.json res/maps/%/tileset.2bpp res/maps/%/tileset.map res/maps/%/tileset.palmap
	@$(MKDIR_P) $(@D)
	$(filter-out $<,$^) "$*" `identify -format '%w' "$<"` >"$@"

res/maps/%/tileset.2bpp: SFC_PAL_FLAGS := -P 128 -C 4


###############################################
#                                             #
#                 COMPILATION                 #
#                                             #
###############################################

# How to build a ROM
$(BINDIR)/%.$(ROMEXT) $(BINDIR)/%.sym $(BINDIR)/%.map: $(patsubst src/%.asm,$(OBJDIR)/%.o,$(SRCS))
	@$(MKDIR_P) $(@D)
	$(RGBASM) $(ASFLAGS) -o $(OBJDIR)/build_date.o src/res/build_date.asm
	$(RGBLINK) $(LDFLAGS) -m $(BINDIR)/$*.map -n $(BINDIR)/$*.sym -o $(BINDIR)/$*.$(ROMEXT) $^ $(OBJDIR)/build_date.o \
	&& $(RGBFIX) -v $(FIXFLAGS) $(BINDIR)/$*.$(ROMEXT)

# `.mk` files are auto-generated dependency lists of the "root" ASM files, to save a lot of hassle.
# Also add all obj dependencies to the dep file too, so Make knows to remake it
# Caution: some of these flags were added in RGBDS 0.4.0, using an earlier version WILL NOT WORK
# (and produce weird errors)
$(OBJDIR)/%.o $(DEPDIR)/%.mk: src/%.asm
	@$(MKDIR_P) $(patsubst %/,%,$(dir $(OBJDIR)/$* $(DEPDIR)/$*))
	$(RGBASM) $(ASFLAGS) -M $(DEPDIR)/$*.mk -MG -MP -MQ $(OBJDIR)/$*.o -MQ $(DEPDIR)/$*.mk -o $(OBJDIR)/$*.o $<

ifeq ($(MAKECMDGOALS),clean)
else ifeq ($(MAKECMDGOALS),mostlyclean)
else
-include $(patsubst src/%.asm,$(DEPDIR)/%.mk,$(SRCS)) $(DEPDIR)/charmap.mk
endif

# Catch non-existent files
# KEEP THIS LAST!!
%:
	@false
