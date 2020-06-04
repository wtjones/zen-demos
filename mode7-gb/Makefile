# Use make -B due to the fact that .inc changes are not getting tracked
# in dependencies.
# Not yet sure how to utilize rgbasm -M (https://rednex.github.io/rgbds/rgbasm.1.html#M)
# to resolve.

ASM = rgbasm
LINK = rgblink
FIX = rgbfix

ROM_NAME    = zen-mode7
SOURCE      := src data
INC_DIR     = include
BUILD_DIR   = build
FIX_FLAGS   = -v -p0
OUTPUT      = $(BUILD_DIR)/$(ROM_NAME)

# List of relative paths to all folders and subfolders with code or data.
SOURCE_ALL_DIRS := $(shell find $(SOURCE) -type d -print)

# All files with extension asm are assembled.
ASMFILES := $(foreach dir,$(SOURCE_ALL_DIRS),$(wildcard $(dir)/*.asm))

OBJ := $(ASMFILES:.asm=.obj)

INCDIR = include
OBJECTS := $(ASMFILES:%.asm=build/%.obj)

.PHONY: all clean

MODE = 0			# default to game mode
test1 : MODE = 1	# test mode 1
test2 : MODE = 2	# test mode 2
test3 : MODE = 3	# test mode 3
test4 : MODE = 4	# test mode 4

all test1 test2 test3 test4: create_build_dir $(OUTPUT)

create_build_dir:
	mkdir -p $(BUILD_DIR)
	mkdir -p $(BUILD_DIR)/src
	mkdir -p $(BUILD_DIR)/data

$(OUTPUT): $(OBJECTS)
	$(LINK) -m $@.map -o $@.gb -n $@.sym $(OBJECTS)
	$(FIX) $(FIX_FLAGS) $@.gb

build/%.obj: %.asm
	$(ASM) -D mode=$(MODE) -i$(INCDIR)/ -o$@ $<

clean:
	rm -rf $(BUILD_DIR)/*
