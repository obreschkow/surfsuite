module module_system

use module_global

contains

function timestamp() result(str)
   implicit none
   integer*4         :: t(8)
   character(len=23) :: str
   call date_and_time(VALUES=t)
   write(str,'(I0,A,I0.2,A,I0.2,A,I0.2,A,I0.2,A,I0.2,A,I0.3)') t(1),'/',t(2),'/',t(3),'-',t(5),':',t(6),':',t(7),'.',t(8)
end function timestamp

function nodot(strin) result(strout)
   ! removed dot from on the RHS of string
   implicit none
   character(*),intent(in) :: strin
   character(len=255)      :: strout
   integer*4               :: l
   l = len(trim(strin))
   if (strin(l:l)=='.') then
      strout = strin(1:l-1)
   else
      strout = strin
   end if
end function nodot

function noslash(strin) result(strout)
   ! removed dot from on the RHS of string
   implicit none
   character(*),intent(in) :: strin
   character(len=255)      :: strout
   integer*4               :: l
   l = len(trim(strin))
   if (strin(l:l)=='/') then
      strout = strin(1:l-1)
   else
      strout = strin
   end if
end function noslash

end module module_system