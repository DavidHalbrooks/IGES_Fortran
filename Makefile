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
CFLAGS += -fPIC -fmax-errors=3 -fcheck=all -fbacktrace
# GDB Flag
CFLAGS += -g
###############################################################################

###############################################################################
# Add source file paths
vpath % .: src
vpath % .: src.de
vpath % .: src.global
vpath % .: src.iges_files
vpath % .: src.obj_defs
vpath % .: src.pd
vpath % .: src.start
vpath % .: src.tail
vpath % .: src.type_defs
vpath % .: src.utils
###############################################################################

###############################################################################
# Find all source files
SRCS := $(wildcard src/*.f90) \
				$(wildcard src/de/*.f90) \
				$(wildcard src/global/*.f90) \
				$(wildcard src/obj_defs/*.f90) \
				$(wildcard src/pd/*.f90) \
				$(wildcard src/start/*.f90) \
				$(wildcard src/tail/*.f90) \
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

#.SUFFIXES : .o .f90

# .f90.o :
# 	$(FC) $(CFLAGS) -c $<

# Compile the source files
$(OBJS): %.o: %
	$(FC) $(CFLAGS) -c -o $@ $< -J./include

# Rebuild all object files in case this Makefile changes
$(OBJS): $(MAKEFILE_LIST)

###############################################################################
# Define dependencies
geometry.mod := src/geometry.f90.o
PD_import.mod := src/pd/PD_import.f90.o
array_utils.mod := src/utils/array_utils.f90.o
obj_T141.mod := src/obj_defs/obj_T141.f90.o
obj_T143.mod := src/obj_defs/obj_T143.f90.o
Type126.mod := src/type_defs/Type126.f90.o
Type128.mod := src/type_defs/Type128.f90.o
Type141.mod := src/type_defs/Type141.f90.o
Type143.mod := src/type_defs/Type143.f90.o
Type314.mod := src/type_defs/Type314.f90.o
count_PD.mod := src/utils/count_PD.f90.o
read_raw_PD.mod := src/utils/read_raw_PD.f90.o
DE_import.mod := src/de/DE_import.f90.o
T_import.mod := src/tail/T_import.f90.o
S_import.mod := src/start/S_import.f90.o
G_import.mod := src/global/G_import.f90.o
open_close.mod := src/utils/open_close.f90.o
num_records.mod := src/utils/num_records.f90.o
filename.mod := src/utils/filename.f90.o

src/utils/read_raw_PD.f90.o: $(count_PD.mod)
src/geometry.f90.o: $(filename.mod)
src/geometry.f90.o: $(open_close.mod)
src/geometry.f90.o: $(T_import.mod)
src/geometry.f90.o: $(S_import.mod)
src/geometry.f90.o: $(G_import.mod)
src/geometry.f90.o: $(DE_import.mod)
src/geometry.f90.o: $(PD_import.mod)
src/geometry.f90.o: $(array_utils.mod)
src/geometry.f90.o: $(obj_T143.mod)
src/geometry.f90.o: $(num_records.mod)
src/obj_defs/obj_T141.f90.o: $(read_raw_PD.mod)
src/obj_defs/obj_T141.f90.o: $(Type126.mod)
src/obj_defs/obj_T141.f90.o: $(Type128.mod)
src/obj_defs/obj_T141.f90.o: $(Type141.mod)
src/obj_defs/obj_T141.f90.o: $(Type143.mod)
src/obj_defs/obj_T141.f90.o: $(Type314.mod)
src/obj_defs/obj_T143.f90.o: $(Type128.mod)
src/obj_defs/obj_T143.f90.o: $(obj_T141.mod)
src/pd/PD_import.f90.o: $(read_raw_PD.mod)
src/pd/PD_import.f90.o: $(Type126.mod)
src/pd/PD_import.f90.o: $(Type128.mod)
src/pd/PD_import.f90.o: $(Type141.mod)
src/pd/PD_import.f90.o: $(Type143.mod)
src/pd/PD_import.f90.o: $(Type314.mod)
src/pd/PD_import.f90.o: $(obj_T143.mod)
src/main.f90.o: $(model.mod)

###############################################################################


###############################################################################
clean:
	$(RM) $(filter %.o, $(OBJS)) $(wildcard *.mod) $(PROG)
###############################################################################


###############################################################################
# Source Files (order matters)
# SRCS := open_close num_records filename  \
# 			 T_import S_import G_import       \
# 			 DE_import                                \
# 			 count_PD_entries read_raw_parametric \
# 			 T126 T128 T141 T143 T314 \
# 			 T141_object T143_object              \
# 			 PD_import resize_array               \
# 			 model main
# OBJS = $(SRCS:=.o)


