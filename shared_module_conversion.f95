! conversion between different variables

module shared_module_conversion

public

contains

integer*4 function log2int(a)

   ! convert logical to int*4
   
   logical, intent(in) :: a
   
   if (a) then
      log2int = 1
   else
      log2int = 0
   end if
   
end function log2int

function val2str(value) result(str)

   ! converts a value of any type into a string
   
   use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
   
   implicit none
   class(*),intent(in)        :: value
   character(len=255)         :: txt
   character(:),allocatable   :: str
   
   select type (value)
   type is (integer(kind=int8));    write(txt,'(i0)') value
   type is (integer(kind=int16));   write(txt,'(i0)') value
   type is (integer(kind=int32));   write(txt,'(i0)') value
   type is (integer(kind=int64));   write(txt,'(i0)') value
   type is (real(kind=real32));     write(txt,'(1pg0)') value
   type is (real(kind=real64));     write(txt,'(1pg0)') value
   type is (real(kind=real128));    write(txt,'(1pg0)') value
   type is (logical);               write(txt,'(1l)') value
   type is (character(*));          write(txt,'(a)') value
   type is (complex(kind=real32));  write(txt,'(1pg0,sp,1pg0,"i")') value
   type is (complex(kind=real64));  write(txt,'(1pg0,sp,1pg0,"i")') value
   type is (complex(kind=real128)); write(txt,'(1pg0,sp,1pg0,"i")') value
   class default
      write(*,'(A)') 'ERROR: unknown type'
      stop
   end select
   
   str = trim(txt)
   
end function val2str
   
end module shared_module_conversion