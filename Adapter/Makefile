# Makefile for compiling Elmer modules with libprecice

# Compiler and flags
FC = elmerf90
PKG_CONFIG = pkg-config
LIBPRECICE_FLAGS = $(shell $(PKG_CONFIG) --libs libprecice)

# Source files and targets
SOURCES = Coupler_Solver.F90 Print_Module.F90 UDF_Boundary.F90
TARGETS = Coupler_Solver.so Print_Module.so UDF_Boundary.so

# Default target to build all shared libraries
all: $(TARGETS)

# Rule for compiling .so files
%.so: %.F90
	$(FC) -o $@ $< $(LIBPRECICE_FLAGS)

# Clean up object files and shared libraries
clean:
	rm -f $(TARGETS)

.PHONY: all clean
