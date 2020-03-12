module module_processhalo

use module_taskhandler
use module_global
use module_system

contains

subroutine center_particles(x0)

   implicit none
   real*4,optional,intent(out)   :: x0(3) ! center of mass position
   integer*4                     :: d
   real*4                        :: h,c
   
   if (present(x0)) x0=0
   
   do d = 1,3
   
      ! wrap coordinates
      h = para%L/2
      if (maxval(p%x(d))-minval(p%x(d))>h) then
         p%x(d) = mod(p%x(d)+h,para%L)
         if (maxval(p%x(d))-minval(p%x(d))>h) call error('ERROR: Halo larger than half the simulation box.')
         if (present(x0)) x0(d)=-h
      end if

      ! reset center of mass
      c = real(sum(real(p%x(d),8))/nparticles,4)
      p%x(d) = p%x(d)-c
      if (present(x0)) x0(d)=x0(d)+c
   
   end do

end subroutine center_particles

end module module_processhalo