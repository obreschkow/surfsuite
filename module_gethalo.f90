module module_gethalo

use module_global
use module_system
use module_io
use module_hdf5
use module_processhalo

private 
public   :: task_gethalo
public   :: nhalos
public   :: load_halo_properties ! only load structure "halo"
public   :: load_halo            ! load structure "halo" as well as particles

contains

subroutine task_gethalo

   implicit none
   integer*4                  :: haloid
   character(len=255)         :: arg_option
   character(len=255)         :: arg_value
   integer*4                  :: i
   logical                    :: output
   character(len=255)         :: outputfile
   integer*4                  :: outputformat ! 1=binary, 2=ascii, 3=hdf5
   integer*4                  :: subhalos,center
   type(type_halo)            :: halo
   
   ! checks
   if (narg<2) then
      call out('ERROR: Argument missing. Use')
      call out('> surfsuite gethalo ID [-outputfile ...] [-outputformat ...] [-subhalos 0/1]')
      stop
   else
      call getarg(2,arg_value)
      read(arg_value,*) haloid
   end if
   
   ! default options
   output = .false.
   outputformat = 1
   subhalos = 0
   center = 0
   
   ! change default options
   if (narg>2) then
      if ((narg+1)/2.eq.(narg+1)/2.0) then
         call out('ERROR: Every option "-option" must have exactly one value.')
         stop
      end if
      do i = 3,narg,2
         call getarg(i,arg_option)
         call getarg(i+1,arg_value)
         select case (trim(arg_option))
         case ('-outputfile')
            output = .true.
            outputfile = trim(arg_value)
         case ('-outputformat')
            read(arg_value,*) outputformat
            if ((outputformat<1).or.(outputformat>3)) then
               call out('Error: outputformat must be 1, 2 or 3.')
               stop
            end if
         case ('-subhalos')
            read(arg_value,*) subhalos
            if ((subhalos<0).or.(subhalos>1)) then
               call out('Error: subhalos must be 0 or 1.')
               stop
            end if
         case ('center')
            read(arg_value,*) center
            if ((center<0).or.(center>1)) then
               call out('Error: center must be 0 or 1.')
               stop
            end if
         end select
      end do
   end if
   
   call load_halo(haloid,subhalos==1,halo)
   
   if (output) then
      if (center==1) call center_particles
      call save_halo(outputfile,outputformat,haloid,halo)
   else
      call hline
      call write_halo_properties(haloid,halo)
      call write_halo_particle_stats
      call hline
   end if
   
end subroutine task_gethalo

integer*4 function nhalos()

   implicit none
   character(len=255)   :: fn
   integer*8            :: filesize
   logical              :: file_exists
    
   fn = trim(para%path_surfsuite)//trim(para%snapshot)//trim(para%ext_halolist)
   inquire(file=trim(fn),exist=file_exists)
   if (.not.file_exists) then
      call out('Error: Could not find file')
      call out(trim(fn))
      stop
   end if
   inquire(file=trim(fn),size=filesize)
   nhalos = int(filesize/int(bytes_per_halo,8),4)

end function nhalos

subroutine load_halo(haloid,include_subhalos,halo)

   implicit none
   integer*4,intent(in)          :: haloid
   logical,intent(in)            :: include_subhalos
   type(type_halo),intent(out)   :: halo

   call load_particles(haloid,include_subhalos)
   call load_halo_properties(haloid,halo)
   
   contains

   recursive subroutine load_particles(haloid,include_subhalos,recursive_call)

      implicit none
      integer*4,intent(in)          :: haloid
      logical,intent(in)            :: include_subhalos
      logical,intent(in),optional   :: recursive_call ! this should only be true when called recursively from within this subroutine
      character(len=255)            :: fn
      integer*8                     :: position
      type(type_halo)               :: halo
      integer*4                     :: i
      
      call load_halo_properties(haloid,halo)
      
      if (.not.present(recursive_call)) then
         if (include_subhalos) then
            nparticles = halo%npart+halo%npartsub
         else
            nparticles = halo%npart
         end if
         if (allocated(p)) deallocate(p)
         allocate(p(nparticles))
         nparticles = 0
      end if
      
      if (halo%npart>0) then
         ! note: there are some halos that have 0 particles, but children with particles
         ! it's a weird feature of the velociraptor outputs
         position = bytes_per_particle*halo%offset+1
         fn = filename(halo%file,para%path_surfsuite,para%snapshot,para%ext_halos)
         open(1,file=trim(fn),action='read',form='unformatted',status='old',access='stream')
         read(1,pos=position) p(nparticles+1)
         do i = 2,halo%npart
            read(1) p(nparticles+i)
         end do
         nparticles = nparticles+halo%npart
      end if
   
      if (include_subhalos) then
         if (halo%firstchildid>-1) call load_particles(halo%firstchildid,include_subhalos,.true.)
         if (present(recursive_call).and.(halo%siblingid>-1)) call load_particles(halo%siblingid,include_subhalos,.true.)
      end if
   
      if (.not.present(recursive_call)) then
         if (nparticles.ne.size(p)) then
            call out('Error in load_halo_particles: n.ne.nparticles')
            stop
         end if
      end if
   
   end subroutine load_particles

end subroutine load_halo

subroutine load_halo_properties(haloid,halo)

   implicit none
   integer*4,intent(in)                   :: haloid
   type(type_halo),intent(out)            :: halo
   character(len=255)                     :: fn
   integer*8                              :: position
   
   if (haloid<1) then
      call out('Error: HaloID below 1:',haloid*1_8)
      stop
   else if (haloid>nhalos()) then
      call out('Error: HaloID too large:',haloid*1_8)
      stop
   end if
   
   fn = trim(para%path_surfsuite)//trim(para%snapshot)//trim(para%ext_halolist)
   position = int(bytes_per_halo,8)*int(haloid-1,8)+1_8
   open(1,file=trim(fn),action='read',form='unformatted',status='old',access='stream')
   read(1,pos=position) halo
   close(1)

end subroutine load_halo_properties

subroutine save_halo(outputfile,outputformat,haloid,halo)

   implicit none
   character(len=255),intent(in) :: outputfile
   integer*4,intent(in)          :: outputformat ! 3=binary, 2=ascii, 1=hdf5
   integer*4,intent(in)          :: haloid
   type(type_halo),intent(in)    :: halo
   integer*8                     :: i
   integer*4                     :: sub
   
   if (outputformat==3) then
   
      open(1,file=trim(outputfile),action='write',form='unformatted',status='replace',access='stream')
      write(1) (p(i),i=1,nparticles)
      close(1)
      
   else if (outputformat==2) then
   
      open(1,file=trim(outputfile),action='write',form='formatted',status='replace')
      write(1,'(I11,I2,3E17.10,3E14.6)') (p(i),i=1,nparticles)
      close(1)
      
   else if (outputformat==1) then
      
      call hdf5_create(trim(outputfile)) ! create HDF5 file
      call hdf5_open(trim(outputfile),.true.) ! open HDF5 file
   
      ! Group "simulation"
      call hdf5_add_group('simulation')
      call hdf5_write_data('simulation/name',trim(para%simulation),'simulation name')
      call hdf5_write_data('simulation/box_l',para%L,'[simulation units] box side length')
      call hdf5_write_data('simulation/box_n',para%N,'cubic root of particle number')
      
      ! Group "surfsuite"
      call hdf5_add_group('surfsuite')
      call hdf5_write_data('surfsuite/version',trim(version),'simulation of surfsuite used to extract the halo')
      call hdf5_write_data('surfsuite/reference','Danail Obreschkow; danail.obreschkow@icrar.org')
      
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
      
      ! Group "particles"
      call hdf5_add_group('particles')
      call hdf5_write_data('particles/id',p%id,'unique particle id')
      call hdf5_write_data('particles/species',p%species,'particle species (1 = gas, 2 = dark matter)')
      call hdf5_write_data('particles/rx',p%x(1),'[simulation units] x-coordinate of position')
      call hdf5_write_data('particles/ry',p%x(2),'[simulation units] y-coordinate of position')
      call hdf5_write_data('particles/rz',p%x(3),'[simulation units] z-coordinate of position')
      call hdf5_write_data('particles/vx',p%v(1),'[simulation units] x-coordinate of velocity')
      call hdf5_write_data('particles/vy',p%v(2),'[simulation units] y-coordinate of velocity')
      call hdf5_write_data('particles/vz',p%v(3),'[simulation units] z-coordinate of velocity')
      
      call hdf5_close() ! close HDF5 file
      
   else
      call out('Error: Unknown outputformat.')
      stop
   end if
end subroutine save_halo

subroutine write_halo_properties(haloid,halo)
   implicit none
   integer*4,intent(in)                   :: haloid
   type(type_halo),intent(in)             :: halo
   character(len=255)                     :: txt
   write(txt,'(A,A)')  'Simulation:    ',trim(para%simulation)
   call out(txt)
   write(txt,'(A,I0)') 'Halo:          ',haloid
   call out(txt)
   write(txt,'(A,I0)') 'File:          ',halo%file
   call out(txt)
   write(txt,'(A,I0)') 'Offset:        ',halo%offset
   call out(txt)
   write(txt,'(A,I0)') 'Npart:         ',halo%npart
   call out(txt)
   write(txt,'(A,I0)') 'Npart sub:     ',halo%npartsub
   call out(txt)
   write(txt,'(A,I0)') 'Parent ID:     ',halo%parentid
   call out(txt)
   write(txt,'(A,I0)') 'Nchidren:      ',halo%nchildren
   call out(txt)
   write(txt,'(A,I0)') '1st child ID:  ',halo%firstchildid
   call out(txt)
   write(txt,'(A,I0)') 'Sibling ID:    ',halo%siblingid
   call out(txt)
end subroutine write_halo_properties

subroutine write_halo_particle_stats
   implicit none
   character(len=255)   :: txt
   character(len=12)    :: wrap
   real*4               :: h
   if (nparticles>0) then
      h = para%L/2
      if (maxval(p%x(1))-minval(p%x(1))>h) then
         wrap = '   (wrapped)'
      else
         wrap = ''
      end if
      write(txt,'(A,F0.4,A,F0.4,A)') 'x-range:       ',minval(p%x(1)),' to ',maxval(p%x(1)),wrap
      call out(txt)
      if (maxval(p%x(2))-minval(p%x(2))>h) then
         wrap = '   (wrapped)'
      else
         wrap = ''
      end if
      write(txt,'(A,F0.4,A,F0.4,A)') 'y-range:       ',minval(p%x(2)),' to ',maxval(p%x(2)),wrap
      call out(txt)
      if (maxval(p%x(3))-minval(p%x(3))>h) then
         wrap = '   (wrapped)'
      else
         wrap = ''
      end if
      write(txt,'(A,F0.4,A,F0.4,A)') 'z-range:       ',minval(p%x(3)),' to ',maxval(p%x(3)),wrap
      call out(txt)
      write(txt,'(A,I0,A,I0,A,I0,A)')     'ID range:      ', &
      &minval(p%id),' to ',maxval(p%id),' (first: ',p(1)%id,')'
      call out(txt)
      write(txt,'(A,I0,A,I0,A,I0,A,I0,A,I0,A,I0,A,I0,A)') &
      & '# particles:   ',nparticles,' (',count(p%species==1),',',count(p%species==2),',',count(p%species==3),',',&
      & count(p%species==4),',',count(p%species==5),',',count(p%species==6),')'
      call out(txt)
   end if
end subroutine write_halo_particle_stats
   
end module module_gethalo