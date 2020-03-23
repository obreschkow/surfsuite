module module_getparticle

use module_taskhandler
use module_global
use module_system
use module_io

implicit none

character(*),parameter  :: module_getparticle_use = '> surfsuite getparticle ID'

contains

subroutine task_getparticle

   implicit none
   integer*8            :: particleid
   
   read(task_value,*) particleid
   call write_particle(particleid)
   
end subroutine task_getparticle

subroutine write_particle(id)

   implicit none
   integer*8,intent(in) :: id
   character(len=255)   :: txt
   type(type_particle)  :: particle
   integer*4            :: ifile
   
   call get_particle(id,particle,ifile)
   call hline
   write(txt,'(A,A)')  'Simulation:  ',trim(para%simulation)
   call out(txt)
   write(txt,'(A,I0)') 'Particle:    ',particle%id
   call out(txt)
   write(txt,'(A,I0)') 'Species:     ',particle%species
   call out(txt)
   write(txt,'(A,I0)') 'File #:      ',ifile
   call out(txt)
   write(txt,'(A,F11.4,A,F11.4,A,F11.4)') 'Position: ',particle%x(1),',',particle%x(2),',',particle%x(3)
   call out(txt)
   write(txt,'(A,F11.4,A,F11.4,A,F11.4)') 'Velocity: ',particle%v(1),',',particle%v(2),',',particle%v(3)
   call out(txt)
   call hline
   
end subroutine write_particle

subroutine get_particle(id,particle,ifile)

   implicit none
   integer*8,intent(in)             :: id
   type(type_particle),intent(out)  :: particle
   integer*4,intent(out),optional   :: ifile
   integer*8                        :: position
   integer*8                        :: file_size
   character(255)                   :: fn
   
   if (id<1) call error('Particle ID below allowed range.')
   
   ifile = int((id-1)/nparticles_per_sorted_file,4)
   position = (id-ifile*nparticles_per_sorted_file-1)*bytes_per_particle+1_8
   
   fn = trim(filename(ifile,para%path_surfsuite,snfile(para%snapshot),para%ext_sorted))
   
   if (.not.exists(trim(fn),.true.)) then
      fn = trim(filename(0,para%path_surfsuite,snfile(para%snapshot),para%ext_sorted))
      if (.not.exists(trim(fn),.true.)) then
         call out('Sorted particle files do not exist for this simulation.')
         call error('Consider running the task "sortparticles".')
      else
         call error('Particle ID outside allowed range or file lost.')
      end if
   end if
   
   inquire(file=trim(fn), size=file_size)
   
   if (position>file_size) call error('Particle ID outside allowed range.')
   
   open(1,file=trim(fn), &
   & action='read',form='unformatted',status='old',access='stream')
   read(1,pos=position) particle
   close(1)

end subroutine get_particle
   
end module module_getparticle