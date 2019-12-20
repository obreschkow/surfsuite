module module_processhalo

use module_global
use module_system

contains

subroutine center_particles

   implicit none
   integer*4   :: d
   real*4      :: h,c

   do d = 1,3
   
      ! wrap coordinates
      h = para%L/2
      if (maxval(p%x(d))-minval(p%x(d))>h) then
         p%x(d) = mod(p%x(d)+h,para%L)
         if (maxval(p%x(d))-minval(p%x(d))>h) then
            call out('ERROR: Halo larger than half the simulation box.')
            stop
         end if
      end if

      ! reset center of mass
      c = real(sum(real(p%x(d),8))/nparticles,4)
      p%x(d) = p%x(d)-c
   
      ! reset center of mass velocity
      c = sum(p%v(d))/real(nparticles,4)
      p%v(d) = p%v(d)-c
      
   end do

end subroutine center_particles

end module module_processhalo