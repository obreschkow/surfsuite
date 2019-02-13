module module_getparticle

use module_global
use module_system
use module_io

implicit none

character(*),parameter  :: module_getparticle_use = '> surfsuite getparticle ID'

contains

subroutine task_getparticle

   implicit none
   integer*8            :: particleid
   character(len=255)   :: arg_value
   
   if (narg<2) then
      call out('ERROR: Argument missing. Use')
      call out(module_getparticle_use)
      stop
   else
      call getarg(2,arg_value)
      read(arg_value,*) particleid
   end if
   
   call hline
   call write_particle(particleid)
   call hline
   
end subroutine task_getparticle

subroutine write_particle(id)

   implicit none
   integer*8,intent(in) :: id
   character(len=255)   :: txt
   type(type_particle)  :: particle
   integer*4            :: ifile
   
   call get_particle(id,particle,ifile)
   write(txt,'(A,A)')  'Simulation:  ',trim(para%simulation)
   call out(txt)
   write(txt,'(A,I0)') 'Particle:    ',particle%id
   call out(txt)
   write(txt,'(A,I0)') 'Species:     ',particle%typ
   call out(txt)
   write(txt,'(A,I0)') 'File #:      ',ifile
   call out(txt)
   write(txt,'(A,F11.4,A,F11.4,A,F11.4)') 'Position: ',particle%x,' ',particle%y,' ',particle%z
   call out(txt)
   write(txt,'(A,F11.4,A,F11.4,A,F11.4)') 'Velocity: ',particle%vx,' ',particle%vy,' ',particle%vz
   call out(txt)
   
end subroutine write_particle

subroutine get_particle(id,particle,ifile)

   implicit none
   integer*8,intent(in)             :: id
   type(type_particle),intent(out)  :: particle
   integer*4,intent(out),optional   :: ifile
   integer*8                        :: position
   integer*8                        :: file_size
   character(255)                   :: fn
   
   if (id<1) then
      call out('Error: Particle ID below allowed range.')
      stop
   end if
   
   ifile = int((id-1)/nparticles_per_sorted_file,4)
   position = (id-ifile*nparticles_per_sorted_file-1)*bytes_per_particle+1_8
   
   fn = trim(filename(ifile,para%path_surfsuite,para%snapshot,para%ext_sorted))
   
   if (.not.exists(trim(fn),.true.)) then
      call out('Error: Particle ID above allowed range or file lost.')
      stop
   end if
   
   inquire(file=trim(fn), size=file_size)
   
   if (position>file_size) then
      call out('Error: Particle ID above allowed range.')
      stop
   end if
   
   open(1,file=trim(fn), &
   & action='read',form='unformatted',status='old',access='stream')
   read(1,pos=position) particle
   close(1)

end subroutine get_particle
   
end module module_getparticle