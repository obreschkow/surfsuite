module module_gethalo

use module_global
use module_system
use module_io

implicit none

character(*),parameter  :: module_gethalo_use = '> surfsuite gethalo ID [-outputfile ...] [-outputformat ...] [-subhalos 0/1]'

contains

subroutine task_gethalo

   implicit none
   integer*4                  :: haloid
   character(len=255)         :: arg_option
   character(len=255)         :: arg_value
   integer*4                  :: i
   logical                    :: output
   character(len=255)         :: outputfile
   integer*4                  :: outputformat ! 1=binary, 2=ascii
   integer*4                  :: subhalos
   type(type_halo)            :: halo
   
   ! checks
   if (narg<2) then
      call out('ERROR: Argument missing. Use')
      call out(module_gethalo_use)
      stop
   else
      call getarg(2,arg_value)
      read(arg_value,*) haloid
   end if
   
   ! default options
   output = .false.
   outputformat = 1
   subhalos = 0
   
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
            if ((outputformat<1).or.(outputformat>2)) then
               call out('Error: outputformat must be 1 or 2.')
               stop
            end if
         case ('-subhalos')
            read(arg_value,*) subhalos
            if ((subhalos<0).or.(subhalos>1)) then
               call out('Error: subhalos must be 0 or 1.')
               stop
            end if
         end select
      end do
   end if
   
   call load_halo_properties(haloid,halo)
   call load_halo_particles(haloid,subhalos==1,.false.)
   
   if (output) then
      call save_halo(outputfile,outputformat)
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

subroutine load_halo_particles(haloid,include_subhalos,center_of_gravity)

   implicit none
   integer*4,intent(in) :: haloid
   logical,intent(in)   :: include_subhalos
   logical,intent(in)   :: center_of_gravity
   real*4               :: r0(3),v0(3)
   real*4               :: dx,dy,dz
   integer*4            :: nmain
   type(type_halo)      :: halo

   call load_particles(haloid,include_subhalos)
   
   call load_halo_properties(haloid,halo)
   nmain = halo%npart
   
   if (center_of_gravity) then
   
      if (maxval(p%x)-minval(p%x)>para%L/2) then
         dx = para%L/2
         p%x = mod(p%x+dx,para%L)
         if (maxval(p%x)-minval(p%x)>para%L/2) then
            call out('ERROR: Halo larger than half the simulation box in x-direction.')
            stop
         end if
      else
         dx = 0
      end if
   
      if (maxval(p%y)-minval(p%y)>para%L/2) then
         dy = para%L/2
         p%y = mod(p%y+dy,para%L)
         if (maxval(p%y)-minval(p%y)>para%L/2) then
            call out('ERROR: Halo larger than half the simulation box in y-direction.')
            stop
         end if
      else
         dy = 0
      end if
   
      if (maxval(p%z)-minval(p%z)>para%L/2) then
         dz = para%L/2
         p%z = mod(p%z+dz,para%L)
         if (maxval(p%z)-minval(p%z)>para%L/2) then
            call out('ERROR: Halo larger than half the simulation box in z-direction.')
            stop
         end if
      else
         dz = 0
      end if
   
      r0(1) = sum(p(1:nmain)%x)/real(nmain,4)
      r0(2) = sum(p(1:nmain)%y)/real(nmain,4)
      r0(3) = sum(p(1:nmain)%z)/real(nmain,4)
      p%x = p%x-r0(1)
      p%y = p%y-r0(2)
      p%z = p%z-r0(3)
   
      ! center of mass velocities
      v0(1) = sum(p(1:nmain)%vx)/real(nmain,4)
      v0(2) = sum(p(1:nmain)%vy)/real(nmain,4)
      v0(3) = sum(p(1:nmain)%vz)/real(nmain,4)
      p%vx = p%vx-v0(1)
      p%vy = p%vy-v0(2)
      p%vz = p%vz-v0(3)
   
   end if

   contains

   recursive subroutine load_particles(haloid,include_subhalos,recursive_call)

      implicit none
      integer*4,intent(in)          :: haloid
      logical,intent(in)            :: include_subhalos
      logical,intent(in),optional   :: recursive_call
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

end subroutine load_halo_particles

subroutine save_halo(outputfile,outputformat)
   character(len=255),intent(in) :: outputfile
   integer*4,intent(in)          :: outputformat ! 1=binary, 2=ascii
   integer*8                     :: i
   if (outputformat==1) then
      open(1,file=trim(outputfile),action='write',form='unformatted',status='replace',access='stream')
      write(1) (p(i),i=1,nparticles)
      close(1)
   else if (outputformat==2) then
      open(1,file=trim(outputfile),action='write',form='formatted',status='replace')
      write(1,'(I11,I2,3E17.10,3E14.6)') (p(i),i=1,nparticles)
      close(1)
   else
      call out('Error: Unknown outputformat.')
      stop
   end if
end subroutine save_halo

subroutine write_halo_properties(haloid,halo)
   implicit none
   integer*4                              :: haloid
   type(type_halo),intent(in)  :: halo
   character(len=255)                     :: txt
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
   if (nparticles>0) then
      write(txt,'(A,F0.4,A,F0.4)') 'x-range:       ',minval(p%x),' to ',maxval(p%x)
      call out(txt)
      write(txt,'(A,F0.4,A,F0.4)') 'y-range:       ',minval(p%y),' to ',maxval(p%y)
      call out(txt)
      write(txt,'(A,F0.4,A,F0.4)') 'z-range:       ',minval(p%z),' to ',maxval(p%z)
      call out(txt)
      write(txt,'(A,I0,A,I0,A,I0,A)')     'ID range:      ', &
      &minval(p%id),' to ',maxval(p%id),' (first: ',p(1)%id,')'
      call out(txt)
      write(txt,'(A,I0,A,I0,A,I0,A,I0,A,I0,A,I0,A,I0,A)') &
      & '# particles:   ',nparticles,' (',count(p%typ==1),',',count(p%typ==2),',',count(p%typ==3),',',&
      & count(p%typ==4),',',count(p%typ==5),',',count(p%typ==6),')'
      call out(txt)
   end if
end subroutine write_halo_particle_stats
   
end module module_gethalo