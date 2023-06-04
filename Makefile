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
Iges_master.mod := src/type_defs/Iges_master.f90.o
num_records.mod := src/utils/num_records.f90.o
filename.mod := src/utils/filename.f90.o
Type_Tail.mod := src/type_defs/Type_Tail.f90.o
Type_Start.mod := src/type_defs/Type_Start.f90.o
Type_Global.mod := src/type_defs/Type_Global.f90.o
open_close.mod := src/utils/open_close.f90.o
record_pos_calcs.mod := src/utils/record_pos_calcs.f90.o
read_global_ascii.mod := src/utils/read_global_ascii.f90.o
Type_Dir_info.mod := src/type_defs/Type_Dir_info.f90.o
Type_Metadata2.mod := src/type_defs/Type_Metadata2.f90.o
Type_Directory.mod := src/type_defs/Type_Directory.f90.o

src/utils/filename.f90.o:
src/utils/num_records.f90.o:
src/utils/record_pos_calcs.f90.o:
src/utils/open_close.f90.o:
src/utils/read_global_ascii.f90.o:
src/type_defs/Type_Tail.f90.o:
src/type_defs/Type_Start.f90.o:
src/type_defs/Type_Dir_info.f90.o:
src/type_defs/Metadata2.f90.o: $(Type_Dir_info.mod)
src/type_defs/Type_Global.f90.o: $(read_global_ascii.mod)
src/type_defs/Iges_master.f90.o: $(filename.mod)
src/type_defs/Iges_master.f90.o: $(open_close.mod)
src/type_defs/Iges_master.f90.o: $(num_records.mod)
src/type_defs/Iges_master.f90.o: $(Type_Tail.mod)
src/type_defs/Iges_master.f90.o: $(Type_Start.mod)
src/type_defs/Iges_master.f90.o: $(record_pos_calcs.mod)
src/type_defs/Iges_master.f90.o: $(Type_Global.mod)
src/type_defs/Iges_master.f90.o: $(Type_Metadata2.mod)
src/type_defs/Iges_master.f90.o: $(Type_Directory.mod)
src/main.f90.o: $(Iges_master.mod)


###############################################################################


###############################################################################
clean:
	$(RM) $(filter %.o, $(OBJS)) $(wildcard *.mod) $(PROG)
###############################################################################
