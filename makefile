# Call as make [system=hyades,...] [mode=standard,dev]

# system = computing system on which stingray is complied and executed
# mode = compilation mode; allowed modes are 'standard' and 'dev'

ifndef system
   system = personal
endif

ifndef mode
   mode = standard
endif

# library flags (depend on the "system" option)
ifeq ($(system),personal) # private laptop of developer Obreschkow
   LFLAGS = -I/usr/local/include -L/usr/local/lib -lhdf5_fortran -lhdf5
else ifeq ($(system),hyades) # in-house cluster at ICRAR/UWA
   LFLAGS = -I${BLDR_HDF5_INCLUDE_PATH} -L${BLDR_HDF5_LIB_PATH} -lhdf5_fortran -lhdf5
else
   $(info ERROR unknown system: '${system}')
stop
endif

# standard compiler flags (depend on the "mode" option)
ifeq ($(mode),standard)
   CFLAGS = -O3 -fopenmp -ffree-line-length-0
else ifeq ($(mode),dev)
   CFLAGS = -O0 -g -fbounds-check -fwhole-file -ffpe-trap=invalid,zero,overflow -Wall -Wunused -Wuninitialized -Wsurprising -Wconversion
else
   $(info ERROR unknown mode: '${mode}')
stop
endif

# concatenate flags
FCFLAGS =  $(CFLAGS) $(LFLAGS)

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
	
%.o: %.f08
	$(FC) $(FCFLAGS) -c $<

# Utility targets
.PHONY: clean veryclean

clean:
	rm -f *.o *.mod *.MOD
	rm -f *~ $(PROGRAMS)
	rm -f fort.*
	rm -f *.bmp