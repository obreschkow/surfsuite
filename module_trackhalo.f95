module module_trackhalo

use module_taskhandler
use module_global
use module_system
use module_io
use module_hdf5
use module_gethalo
use module_processhalo

implicit none

private
public   :: task_trackhalo
public   :: load_halo_evolving_particles  ! load evolving particle data of a halo

contains

subroutine task_trackhalo

   implicit none
   
   integer*4            :: i
   integer*4            :: haloid
   integer*4            :: subhalos = 0
   integer*4            :: center = 1
   integer*4            :: snapshot_max = -1
   integer*4            :: snapshot_min = -1
   character(len=255)   :: outputfile = ''
   real*4,allocatable   :: x(:,:,:)
   real*4,allocatable   :: v(:,:,:)
   type(type_halo)      :: halo
   
   read(task_value,*) haloid
   
   ! handle options
   do i = 1,n_options
      select case (trim(option_name(i)))
      case ('-outputfile')
         call using_option(i)
         outputfile = trim(option_value(i))
      case ('-from')
         call using_option(i)
         read(option_value(i),*) snapshot_min
         if (snapshot_min<0) call error('argument of -from must be >=0.')
      case ('-to')
         call using_option(i)
         read(option_value(i),*) snapshot_max
         if (snapshot_max<0) call error('argument of -to must be >=0.')
      case ('-subhalos')
         call using_option(i)
         read(option_value(i),*) subhalos
         if ((subhalos<0).or.(subhalos>1)) call error('Error: subhalos must be 0 or 1.')
      case ('-center')
         call using_option(i)
         read(option_value(i),*) center
         if ((center<0).or.(center>1)) call error('Error: center must be 0 or 1.')
      end select
   end do
   call require_no_options_left
   
   if (trim(outputfile)=='') call error('argument -outputfile must be specified.')
   if (snapshot_min<0) call error('arguemnt -from must best specified.')
   if (snapshot_max<0) call error('arguemnt -to must best specified.')
   if (snapshot_min>snapshot_max) call error('snapshot index "from" must be lower or equal than snapshot index "to".')
   
   call hline
   call tic()
   call load_halo_properties(haloid,halo)
   call load_halo_evolving_particles(haloid,subhalos==1,center==1,snapshot_min,snapshot_max,x,v)
   call save_evolving_particles
   call toc()
   
   contains
    
   subroutine save_evolving_particles

      implicit none
      integer*4            :: sub,sn
      character(len=255)   :: snstr
   
      call hdf5_create(trim(outputfile)) ! create HDF5 file
      call hdf5_open(trim(outputfile),.true.) ! open HDF5 file

      ! Group "simulation"
      call hdf5_add_group('simulation')
      call hdf5_write_data('simulation/name',trim(para%simulation),'simulation name')
      call hdf5_write_data('simulation/box_l',para%L,'[simulation units] box side length')
      call hdf5_write_data('simulation/box_n',para%N,'cubic root of particle number')
      call hdf5_write_data('simulation/snapshot',para%snapshot,'snapshot index')
      
      ! Group "surfsuite"
      call hdf5_add_group('surfsuite')
      call hdf5_write_data('surfsuite/timestamp',timestamp(),'surfsuite timestamp')
      call hdf5_write_data('surfsuite/version',trim(version),'version of surfsuite used to extract the halo')
      call hdf5_write_data('surfsuite/developer','Danail Obreschkow; danail.obreschkow@icrar.org')
   
      ! Group "halo"
      call hdf5_add_group('halo')
      call hdf5_write_data('halo/id',haloid,'unique halo id')
      call hdf5_write_data('halo/parentid',halo%parentid,'id of parent halo')
      call hdf5_write_data('halo/n_subhalos',halo%nchildren,'number of subhalos')
      call hdf5_write_data('halo/file',halo%file,'surfsuite file number')
      call hdf5_write_data('halo/n_particles_main',halo%npart,'number of particles in main halo')
      call hdf5_write_data('halo/n_particles',nparticles,'number of particles in file')
      if (nparticles==halo%npart) then
         sub = 0
      else
         sub = 1
      end if
      call hdf5_write_data('halo/includes_subhalos',sub,'logical flag (0/1 = subhalo particles are included/excluded)')
       
      ! Group "tracking"
      call hdf5_add_group('tracking')
      call hdf5_write_data('tracking/snapshot_min',snapshot_min,'minimal snapshot index of track')
      call hdf5_write_data('tracking/snapshot_max',snapshot_max,'maximal snapshot index of track')
      call hdf5_write_data('tracking/center',center, & 
      & 'logical flag specifying if positions and velocities are centered to main snapshot (0=false, 1=true)')
   
      ! Group "particles"
      call hdf5_add_group('particles')
      call hdf5_write_data('particles/id',p%id,'unique particle id')
      call hdf5_write_data('particles/species',p%species,'particle species (1 = gas, 2 = dark matter)')
      do sn = snapshot_min,snapshot_max
         snstr = trim(snfile(sn))
         call hdf5_add_group('particles/'//trim(snstr))
         call hdf5_write_data('particles/'//trim(snstr)//'/rx',x(:,1,sn),'[simulation units] x-coordinate of position')
         call hdf5_write_data('particles/'//trim(snstr)//'/ry',x(:,2,sn),'[simulation units] y-coordinate of position')
         call hdf5_write_data('particles/'//trim(snstr)//'/rz',x(:,3,sn),'[simulation units] z-coordinate of position')
         call hdf5_write_data('particles/'//trim(snstr)//'/vx',v(:,1,sn),'[simulation units] x-coordinate of velocity')
         call hdf5_write_data('particles/'//trim(snstr)//'/vy',v(:,2,sn),'[simulation units] y-coordinate of velocity')
         call hdf5_write_data('particles/'//trim(snstr)//'/vz',v(:,3,sn),'[simulation units] z-coordinate of velocity')
      end do
      
      call hdf5_close() ! close HDF5 file
      
   end subroutine save_evolving_particles
   
end subroutine task_trackhalo
   
subroutine load_halo_evolving_particles(haloid,include_subhalos,center,snapshot_min,snapshot_max,x,v)

   implicit none
   integer*4,intent(in)             :: haloid
   logical,intent(in)               :: include_subhalos
   logical,intent(in)               :: center
   integer*4,intent(in)             :: snapshot_min,snapshot_max ! snapshot range
   real*4,allocatable,intent(out)   :: x(:,:,:)
   real*4,allocatable,intent(out)   :: v(:,:,:)
   integer*4                        :: n,i
   integer*4                        :: ifileold,sn
   integer*8                        :: id,j
   character(255)                   :: fn
   type(type_particle)              :: particle
   integer*4,allocatable            :: ifile(:)
   integer*8,allocatable            :: position(:),list(:,:)
   real*4                           :: x0(3)

   call load_halo_particles(haloid,include_subhalos)
   if (center) then
      call center_particles(x0)
   else
      x0 = 0
   end if

   n = size(p)

   allocate(x(n,3,snapshot_min:snapshot_max))
   allocate(v(n,3,snapshot_min:snapshot_max))
   allocate(ifile(n),position(n),list(n,2))

   ! determine file-id and position within file for each particle
   do i = 1,n
   
      id = p(i)%id
      ifile(i) = int((id-1)/nparticles_per_sorted_file,4)
      position(i) = (id-ifile(i)*nparticles_per_sorted_file-1)*bytes_per_particle+1_8
      list(i,1) = ifile(i)*1000000000000_8+position(i)
      list(i,2) = i
   
   end do

   ! order particles by increasing file id and position id within each file id
   call merge_sort_list(list)

   ! retrieve particle information
   do sn = snapshot_min,snapshot_max
   
      call out('Process '//trim(snfile(sn)))

      ifileold = -1

      do i = 1,n
   
         j = list(i,2)
      
         if (ifile(j).ne.ifileold) then
            close(1)
            fn = trim(filename(ifile(j),para%path_surfsuite,trim(snfile(sn)),para%ext_sorted))
            if (exists(fn)) open(1,file=trim(fn), &
            & action='read',form='unformatted',status='old',access='stream')
            ifileold = ifile(j)
         end if
      
         read(1,pos=position(j)) particle
         x(i,:,sn) = mod(particle%x-x0,para%L)
         v(i,:,sn) = particle%v
         
      end do
   
      close(1)

   end do

   contains

   recursive subroutine merge_sort_list(list,level)

      ! sorts n rows of the n-by-2 matrix list(1:n,1:2) in increasing order of the first column

      implicit none
      integer*8,intent(inout)       :: list(:,:)
      integer,intent(in),optional   :: level
      integer*8,allocatable         :: list1(:,:),list2(:,:)
      integer*4                     :: n0,n1,n2,i0,i1,i2
      integer*4                     :: next_level

      if (present(level)) then
         next_level = level+1
      else
         next_level = 1
      end if

      n0 = size(list(:,1))

      ! split list in 2 sublists
      n1 = n0/2
      n2 = n0-n1
      allocate(list1(n1,2),list2(n2,2))
      list1(1:n1,:) = list(1:n1,:)
      list2(1:n2,:) = list(n0/2+1:n0,:)

      if (n1>1) call merge_sort_list(list1,next_level)
      if (n2>1) call merge_sort_list(list2,next_level)

      ! merge sorted sublists
      i0 = 1
      i1 = 1
      i2 = 1
      do while ((i1<=n1) .and. (i2<=n2))
         if (list1(i1,1)<list2(i2,1)) then
            list(i0,:) = list1(i1,:)
            i1 = i1+1
         else
            list(i0,:) = list2(i2,:)
            i2 = i2+1
         end if
         i0 = i0+1
      end do
      if (i1<=n1) then
         list(i0:i0+n1-i1,:) = list1(i1:n1,:)
      else if (i2<=n2) then
         list(i0:i0+n2-i2,:) = list2(i2:n2,:)
      end if

   end subroutine merge_sort_list

end subroutine load_halo_evolving_particles
   
end module module_trackhalo