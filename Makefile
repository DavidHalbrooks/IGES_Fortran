MAKEFLAGS += --no-builtin-rules --no-builtin-variables

# Program Name
PROG := import

###############################################################################
# Configuration
FC := gfortran
AR := ar rcs
LD := $(FC)
RM := rm -f
###############################################################################

###############################################################################
# Compiler Flags

# Standard
CFLAGS = -std=f2008ts
# Warning Flags
CFLAGS += -Wall
# Debugging Options
CFLAGS += -fPIC -fmax-errors=3 -fcheck=all -fbacktrace -fbounds-check
# GDB Flag
CFLAGS += -g
###############################################################################

###############################################################################
# Add source file paths
vpath % .: src
vpath % .: src.type_defs
vpath % .: src.utils
###############################################################################

###############################################################################
# Find all source files
SRCS := $(wildcard src/*.f90) \
				$(wildcard src/type_defs/*.f90) \
				$(wildcard src/utils/*.f90)
###############################################################################

# Recursevely Define a map from each file to its object file
obj = $(src).o
$(foreach src, $(SRCS), $(eval $(src) := $(obj)))

# Create lists of the build artefacts in this project
OBJS := $(addsuffix .o, $(SRCS))

###############################################################################

.PHONY: all clean test

# Compile Project
all : $(PROG)
	@echo Model Compiled

# Link the object files
$(PROG) : $(OBJS)
	$(LD) $(CFLAGS) -o $@ $^

# Compile the source files
$(OBJS): %.o: %
	$(FC) $(CFLAGS) -c -o $@ $< -J./include

# Rebuild all object files in case this Makefile changes
$(OBJS): $(MAKEFILE_LIST)

###############################################################################
# Define dependencies

# type_defs dir
iges_master.mod := src/type_defs/iges_master.f90.o
type_tail.mod := src/type_defs/type_tail.f90.o
type_start.mod := src/type_defs/type_start.f90.o
type_global.mod := src/type_defs/type_global.f90.o
type_dir_meta.mod := src/type_defs/type_dir_meta.f90.o
type_metadata2.mod := src/type_defs/type_metadata2.f90.o
type_directory.mod := src/type_defs/type_directory.f90.o
type_143.mod := src/type_defs/type_143.f90.o
type_128.mod := src/type_defs/type_128.f90.o
type_141.mod := src/type_defs/type_141.f90.o

# utils dir
open_close.mod := src/utils/open_close.f90.o
filename.mod := src/utils/filename.f90.o
num_records.mod := src/utils/num_records.f90.o
record_pos_calcs.mod := src/utils/record_pos_calcs.f90.o
read_global_ascii.mod := src/utils/read_global_ascii.f90.o
count_pd.mod := src/utils/count_pd.f90.o
num_records.mod := src/utils/num_records.f90.o
read_ascii_pd.mod := src/utils/read_ascii_pd.f90.o
record_pos_calcs.mod := src/utils/record_pos_calcs.f90.o

# No dependencies
src/utils/filename.f90.o:
src/utils/num_records.f90.o:
src/utils/record_pos_calcs.f90.o:
src/utils/open_close.f90.o:
src/utils/read_global_ascii.f90.o:
src/type_defs/type_tail.f90.o:
src/type_defs/type_start.f90.o:
src/type_defs/type_dir_meta.f90.o:

# type_global deps
src/type_defs/type_global.f90.o: $(read_global_ascii.mod)

# metadata2 deps
src/type_defs/metadata2.f90.o: $(type_dir_meta.mod)

# iges_master deps
src/type_defs/iges_master.f90.o: $(filename.mod)
src/type_defs/iges_master.f90.o: $(open_close.mod)
src/type_defs/iges_master.f90.o: $(num_records.mod)
src/type_defs/iges_master.f90.o: $(type_tail.mod)
src/type_defs/iges_master.f90.o: $(type_start.mod)
src/type_defs/iges_master.f90.o: $(record_pos_calcs.mod)
src/type_defs/iges_master.f90.o: $(type_global.mod)
src/type_defs/iges_master.f90.o: $(type_metadata2.mod)
src/type_defs/iges_master.f90.o: $(type_directory.mod)

# main deps
src/main.f90.o: $(iges_master.mod)


###############################################################################


###############################################################################
clean:
	$(RM) $(filter %.o, $(OBJS)) $(wildcard *.mod) $(PROG)
	$(RM) $(wildcard ./include/*.mod)
###############################################################################
