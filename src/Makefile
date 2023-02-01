# Compiler
FC = gfortran

# Compiler Flags
# Standard
CFLAGS = -std=f2008ts

# Warning Flags
CFLAGS += -Wall

# Debugging Options
CFLAGS += -fPIC -fmax-errors=3 -fcheck=all -fbacktrace

# GDB Flag
CFLAGS += -g

# Root filenames
BASE_NAME =Model

# Source Files (order matters)
SRCS = mod_open_close mod_num_records mod_filename  \
			 mod_T_import mod_S_import mod_G_import       \
			 mod_DE_import                                \
			 mod_count_PD_entries mod_read_raw_parametric \
			 mod_T126 mod_T128 mod_T141 mod_T143 mod_T314 \
			 mod_T141_object mod_T143_object              \
			 mod_PD_import mod_resize_array               \
			 mod_$(BASE_NAME) main_$(BASE_NAME)
OBJS = $(SRCS:=.o)

# Executable
MAIN = $(BASE_NAME)

# Compile Project
all : $(MAIN)
	@echo Model Compiled

$(MAIN) : $(OBJS)
	$(FC) $(CFLAGS) -o $(MAIN) $(OBJS)

.SUFFIXES : .o .f90

.f90.o :
	$(FC) $(CFLAGS) -c $<

clean :
	$(RM) *.o *.mod $(MAIN)




