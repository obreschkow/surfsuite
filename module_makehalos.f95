module module_makehalos

use module_global
use module_system
use module_io
use module_hdf5
use module_getparticle

private
public   :: task_makehalos

integer*4               :: nfiles_velociraptor
integer*4               :: nfiles_sorted_particles

contains

subroutine task_makehalos

   implicit none
   character(len=255)      :: fnbase
   
   ! get numbers of files
   fnbase = trim(para%path_surfsuite)//trim(para%snapshot)//trim(para%ext_sorted)
   nfiles_sorted_particles = get_number_of_subfiles(trim(fnbase))
   fnbase = trim(para%path_velociraptor)//trim(para%snapshot)//trim(para%ext_particles)
   nfiles_velociraptor = get_number_of_subfiles(trim(fnbase))
   
   ! basic file check
   fnbase = trim(para%path_velociraptor)//trim(para%snapshot)//trim(para%ext_groups)
   if (nfiles_velociraptor.ne.get_number_of_subfiles(trim(fnbase))) then
      call out('Error: unequal numbers of particle and group files.')
      stop
   end if
   
   call tic_total
   call makehalolist
   call makehalos
   call toc_total

end subroutine task_makehalos

subroutine makehalolist

   implicit none
   integer*4               :: i,nhalos
   integer*8               :: j
   integer*4,allocatable   :: firstposition(:)
   integer*4,allocatable   :: parentid(:)
   integer*4,allocatable   :: npart(:)
   character(len=255)      :: fnbase,fnbase2,fn
   integer*4               :: nhalos_tot,npart_file
   integer*8               :: npart_tot
   
   call out('MAKE HALO LIST WITH PARTICLE POINTERS')
   
   call out('Number of VELOCIraptor group files:',nfiles_velociraptor*1_8)
   
   fnbase  = trim(para%path_velociraptor)//trim(para%snapshot)//trim(para%ext_groups)
   fnbase2 = trim(para%path_velociraptor)//trim(para%snapshot)//trim(para%ext_particles)
   
   open(1,file=trim(para%path_surfsuite)//trim(para%snapshot)//trim(para%ext_halolist),&
   &action='write',form='unformatted',status='replace',access='stream')
   
   nhalos_tot = 0
   npart_tot = 0
   do i = 0,nfiles_velociraptor-1
   
      
      ! determine number of particles in file
      fn = filename(i,fnbase2)
      call hdf5_open(trim(fn))
      call hdf5_read_data('Num_of_particles_in_groups',npart_file,.true.)
      if (npart_file==huge(npart_file)) then
         call out('Error in makehalolist: number of particles in file too large.')
         stop
      end if
      
      ! read velociraptor group catalog
      fn = filename(i,fnbase)
      call hdf5_open(trim(fn))
      call hdf5_read_data('Num_of_groups',nhalos,.true.)
      allocate(firstposition(nhalos))
      allocate(parentid(nhalos))
      call hdf5_read_data('Offset',firstposition,.true.)
      call hdf5_read_data('Parent_halo_ID',parentid,.true.)
      call hdf5_close()
      
      ! determine number of particles per group
      allocate(npart(nhalos))
      npart(1:nhalos-1) = firstposition(2:nhalos)-firstposition(1:nhalos-1)
      npart(nhalos) = npart_file-firstposition(nhalos)
      if (minval(npart)<0) then
         call out('Error: something went wrong in reading the VELOCIraptor group files,')
         call out('as there  are halos with a negative number of particles.')
         close(1)
         stop
      end if
      
      ! adjust parent IDs
      do j = 1,nhalos
         if (parentid(j)>-1) parentid(j) = parentid(j)+nhalos_tot
      end do
      
      ! save halo list
      do j = 1,nhalos
         write(1) i,firstposition(j),npart(j),parentid(j)
      end do
      
      ! sum total number of halos and particles
      nhalos_tot = nhalos_tot+nhalos
      npart_tot = npart_tot+sum(npart)
      
      ! deallocate arrays
      deallocate(firstposition)
      deallocate(parentid)
      deallocate(npart)
      
   end do
   
   close(1)
   
   call complete_families
   
   call out('Total number of halos:',nhalos_tot*1_8)
   call out('Total number of particles in halos:',npart_tot)
   
   call toc_total
   
   contains
   
   subroutine complete_families
   
      implicit none
      type(type_halo),allocatable :: halo(:)
      integer*4                              :: i,index,child,parent
      integer*8                              :: ncheck
      
      ! load halo list
      allocate(halo(nhalos_tot))
      open(1,file=trim(para%path_surfsuite)//trim(para%snapshot)//trim(para%ext_halolist),&
      &action='read',form='unformatted',access='stream')
      do i = 1,nhalos_tot
         halo(i)%nchildren = 0
         halo(i)%firstchildid = -1
         halo(i)%siblingid = -1
         halo(i)%npartsub = 0
         read(1) halo(i)%file,halo(i)%offset,halo(i)%npart,halo(i)%parentid
      end do
      close(1)
      
      ! add nchildren, firstchildid, siblingid
      do i = 1,nhalos_tot
         if (halo(i)%parentid>0) then
            index = halo(i)%parentid
            if (halo(index)%nchildren>0) halo(i)%siblingid = halo(index)%firstchildid
            halo(index)%firstchildid = i
            halo(index)%nchildren = halo(index)%nchildren+1
         end if
      end do
      
      ! count number of particles in sub-halos of all generations
      do i = 1,nhalos_tot
         child = i
         parent = halo(child)%parentid
         do while (parent>0)
            halo(parent)%npartsub = halo(parent)%npartsub+halo(i)%npart
            child = parent
            parent = halo(child)%parentid
         end do
      end do
      
      ! check
      ncheck = 0
      do i = 1,nhalos_tot
         if (halo(i)%parentid==-1) ncheck = ncheck+halo(i)%npart+halo(i)%npartsub
      end do
      if (sum(int(halo%npart,8)).ne.ncheck) then
         call out('Error: subhalo count error')
         stop
      end if
      
      ! save halo list
      open(1,file=trim(para%path_surfsuite)//trim(para%snapshot)//trim(para%ext_halolist),&
      &action='write',form='unformatted',status='replace',access='stream')
      do i = 1,nhalos_tot
         write(1) halo(i)
      end do
      close(1)
   
   end subroutine complete_families

end subroutine makehalolist

subroutine makehalos

   implicit none
   integer*4               :: i
   integer*8               :: j,n
   character(len=255)      :: fnbase,fn
   integer*8,allocatable   :: array(:)
   integer*8               :: position,positionold
   integer*4               :: ifile,ifileold
   type(type_particle)     :: particle
   integer*4               :: npart_file
   
   call tic
   call out('INITIALIZE INPUT FILES')
   
   fnbase = trim(para%path_velociraptor)//trim(para%snapshot)//trim(para%ext_particles)
   
   call out('Number of VelociRaptor particle files:',nfiles_velociraptor*1_8)
   
   ! open all sorted particle files
   ifileold = -1
   positionold = -1
   do ifile = 0,nfiles_sorted_particles-1
      fn = trim(filename(ifile,para%path_surfsuite,para%snapshot,para%ext_sorted))
      if (exists(trim(fn))) then
         open(ifile+1000,file=trim(fn), &
         & action='read',form='unformatted',status='old',access='stream')
      end if
   end do
   call toc
   
   n = 0
   do i = 0,nfiles_velociraptor-1
      
      call out('WRITE HALO FILE ',i*1_8)
      call tic
      
      ! read particles IDs
      fn = filename(i,fnbase)
      call out('Load file '//trim(fn))
      call hdf5_open(trim(fn))
      call hdf5_read_data('Num_of_particles_in_groups',npart_file,.true.)
      if (npart_file==huge(npart_file)) then
         call out('Error in makehalolist: number of particles in file too large.')
         stop
      end if
      allocate(array(npart_file))
      call hdf5_read_data('Particle_IDs',array)
      call hdf5_close()
      
      n = n+npart_file
      
      fn = filename(i,para%path_surfsuite,para%snapshot,para%ext_halos,multi=nfiles_velociraptor>1)
      call out('Write file '//trim(fn))
      open(1,file=trim(fn),action='write',form='unformatted',status='replace',access='stream')
      do j = 1,size(array)
         ifile = int((array(j)-1)/nparticles_per_sorted_file,4)
         position = (array(j)-ifile*nparticles_per_sorted_file-1)*bytes_per_particle+1_8
         if ((ifileold==ifile).and.(positionold+bytes_per_particle==position)) then
            read(ifile+1000) particle
         else
            read(ifile+1000,pos=position) particle
         end if
         ifileold = ifileold
         positionold = position
         if (particle%id.ne.array(j)) then
            call out('Error: something goes wrong in reading sorted particles based on')
            call out('the VELOCIraptor particle files.')
            stop
         end if
         write(1) particle
      end do
      close (1)
      deallocate(array)
      
      call toc
      
   end do
   
   do ifile = 0,nfiles_sorted_particles-1
      close(ifile)
   end do
   
   call out('Total number of particles in halos:',n*1_8)

end subroutine makehalos

end module module_makehalos