# compiler
FC = gfortran

# compiler flags (try dfferent ones, if the default line does not work)
#FCFLAGS = -g -O0 -L/usr/local/lib -llapack -lblas -I/usr/local/include -L/usr/local/lib -lhdf5_fortran -lhdf5 -fbounds-check -fwhole-file -ffpe-trap=invalid,zero,overflow,underflow -Wall -Wunused -Wuninitialized -Wsurprising -Wconversion
FCFLAGS = -O3 -fopenmp -L/usr/local/lib -llapack -lblas -I/usr/local/include -L/usr/local/lib -lhdf5_fortran -lhdf5

# List of executables to be built within the package
PROGRAMS = surfsuite

# "make" builds all
all: $(PROGRAMS)

surfsuite.o:   module_global.o \
               module_system.o \
               module_hdf5.o \
               module_io.o \
               module_sortparticles.o \
               module_getparticle.o \
               module_makehalos.o \
               module_gethalo.o \
               module_renderhalo.o \
               module_analysis.o
surfsuite: 	   module_global.o \
               module_system.o \
               module_hdf5.o \
               module_io.o \
               module_sortparticles.o \
               module_getparticle.o \
               module_makehalos.o \
               module_gethalo.o \
               module_renderhalo.o \
               module_analysis.o

# ======================================================================
# And now the general rules, these should not require modification
# ======================================================================

# General rule for building prog from prog.o; $^ (GNU extension) is
# used in order to list additional object files on which the
# executable depends
%: %.o
	$(FC) $(FCFLAGS) -o $@ $^

# General rules for building prog.o from prog.f90 or prog.F90; $< is
# used in order to list only the first prerequisite (the source file)
# and not the additional prerequisites such as module or include files
%.o: %.f90
	$(FC) $(FCFLAGS) -c $<
	
%.o: %.f95
	$(FC) $(FCFLAGS) -c $<

%.o: %.f03
	$(FC) $(FCFLAGS) -c $<

# Utility targets
.PHONY: clean veryclean

clean:
	rm -f *.o *.mod *.MOD
	rm -f *~ $(PROGRAMS)
	rm -f fort.*
	rm -f *.bmp