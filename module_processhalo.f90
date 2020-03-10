module module_processhalo

use module_taskhandler
use module_global
use module_system

contains

subroutine center_particles(shiftx,shiftv)

   implicit none
   real*4,optional,intent(out)   :: shiftx(3),shiftv(3)
   integer*4                     :: d
   real*4                        :: h,c
   
   if (present(shiftx)) shiftx=0
   if (present(shiftv)) shiftv=0

   do d = 1,3
   
      ! wrap coordinates
      h = para%L/2
      if (maxval(p%x(d))-minval(p%x(d))>h) then
         p%x(d) = mod(p%x(d)+h,para%L)
         if (maxval(p%x(d))-minval(p%x(d))>h) call error('ERROR: Halo larger than half the simulation box.')
         if (present(shiftx)) shiftx(d)=h
      end if

      ! reset center of mass
      c = real(sum(real(p%x(d),8))/nparticles,4)
      p%x(d) = p%x(d)-c
      if (present(shiftx)) shiftx(d)=shiftx(d)-c
   
      ! reset center of mass velocity
      c = sum(p%v(d))/real(nparticles,4)
      p%v(d) = p%v(d)-c
      if (present(shiftv)) shiftv(d)=-c
      
   end do

end subroutine center_particles

end module module_processhalo