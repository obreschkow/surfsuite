module module_global

   character(*),parameter  :: version = '0.7'

   type type_para
      character(len=255)   :: parameterfile
      character(len=255)   :: simulation
      character(len=255)   :: snapshot
      real*4               :: L ! box side length in simulation units
      integer*4            :: N ! number of particles on a side
      character(len=255)   :: path_gadget
      character(len=255)   :: path_velociraptor
      character(len=255)   :: path_surfsuite
      character(len=255)   :: path_analysis
      character(len=255)   :: ext_groups
      character(len=255)   :: ext_particles
      character(len=255)   :: ext_sorted
      character(len=255)   :: ext_halos
      character(len=255)   :: ext_halolist
   end type type_para

   type(type_para)         :: para

   type type_halo
      integer*4   :: file
      integer*4   :: offset
      integer*4   :: npart    ! number of particles in halo without subhalos
      integer*4   :: npartsub ! number or particles in subhalos (without parent)
      integer*4   :: parentid
      integer*4   :: nchildren
      integer*4   :: firstchildid
      integer*4   :: siblingid
   end type type_halo
   
   type type_particle
      integer*8            :: id          ! unique identifiers
      integer*4            :: typ         ! particle type
      real*4               :: x,y,z       ! positions
      real*4               :: vx,vy,vz    ! velocities
   end type type_particle

   integer*4,parameter     :: bytes_per_particle = 36 ! bytes per instance of type_particle
   integer*4,parameter     :: bytes_per_halo = 32 ! bytes per instance of type_halo
   integer*8,parameter     :: nparticles_per_sorted_file = 100000000_8

   ! input arguments
   integer*4               :: narg

   ! particle properties of single sub-snapshot
   type(type_particle),allocatable  :: p(:)
   integer*8                        :: nparticles           ! number of currently loaded particles, stored in the above arrays

end module module_global