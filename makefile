# Call as make [system=hyades,...] [mode=standard,dev]

# system = computing system on which stingray is complied and executed
# mode = compilation mode; allowed modes are 'standard' and 'dev'

ifndef system
   system = ems
endif

ifndef mode
   mode = standard
endif

cflags = empty
ifeq ($(mode),standard)
   cflags = -O3 -fopenmp
endif
ifeq ($(mode),dev)
   cflags = -O0 -fbounds-check -fcheck=bounds -fwhole-file -ffpe-trap=invalid,zero,overflow -Wall -Wunused -Wuninitialized -Wsurprising -Wconversion
endif
ifeq ($(cflags),empty)
   $(info ERROR unknown mode: '${mode}')
stop
endif

# custom flags to load the HDF5 library
sflags = empty
ifeq ($(system),ems) # private laptop of developer Obreschkow
   sflags = -I/usr/local/include -L/usr/local/lib -lhdf5_fortran -lhdf5
endif
ifeq ($(system),ism49) # private backup laptop of developer Obreschkow
   sflags = -I/usr/local/lib/hdf5/include -L/usr/local/lib/hdf5/lib -lhdf5_fortran -lhdf5 -L/usr/local/lib
endif
ifeq ($(system),hyades) # in-house cluster at ICRAR/UWA
   sflags = -I/opt/bldr/local/storage/hdf5/1.10.2/include -L/opt/bldr/local/storage/hdf5/1.10.2/lib -lhdf5_fortran -lhdf5
endif
ifeq ($(sflags),empty)
   $(info ERROR unknown system: '${system}')
stop
endif

# concatenate compiler flags
FCFLAGS = -g $(sflags) $(cflags)

# Compiler
FC = gfortran

# user info
$(info Compilation options:)
$(info + Computing system = '${system}'.)
$(info + Compiling mode = '${mode}'.)

# List of executables to be built within the package
PROGRAMS = surfsuite

# "make" builds all
all: $(PROGRAMS)

surfsuite.o:   shared_module_core.o \
               shared_module_arguments.o \
               shared_module_parameters.o \
               shared_module_hdf5.o \
               shared_module_graphics.o \
               module_global.o \
               module_io.o \
               module_sortparticles.o \
               module_getparticle.o \
               module_makehalos.o \
               module_processhalo.o \
               module_gethalo.o \
               module_trackhalo.o \
               module_showhalo.o
surfsuite: 	   shared_module_core.o \
               shared_module_arguments.o \
               shared_module_parameters.o \
               shared_module_hdf5.o \
               shared_module_graphics.o \
               module_global.o \
               module_io.o \
               module_sortparticles.o \
               module_getparticle.o \
               module_processhalo.o \
               module_makehalos.o \
               module_gethalo.o \
               module_trackhalo.o \
               module_showhalo.o

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