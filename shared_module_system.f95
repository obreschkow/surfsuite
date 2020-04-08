! frequently used system functions

module shared_module_system

public

contains

logical function exists(filename)
   ! check if file exists; NB: use checkfile routine in shared_module_interface to produce an error if the file does not exist
   implicit none
   character(len=*),intent(in)   :: filename
   inquire(file=trim(filename), exist=exists)
end function exists

logical function isempty(str)
   implicit none
   character(*),intent(in) :: str
   isempty = len(trim(str))==0
end function isempty

function timestamp() result(str)
   ! human readable time stamp
   implicit none
   integer*4         :: t(8)
   character(len=23) :: str
   call date_and_time(VALUES=t)
   write(str,'(I0,A,I0.2,A,I0.2,A,I0.2,A,I0.2,A,I0.2,A,I0.3)') t(1),'/',t(2),'/',t(3),'-',t(5),':',t(6),':',t(7),'.',t(8)
end function timestamp

function noslash(strin,symbol) result(strout)
   ! remove forward-slash (or symbol specified by "symbol") from on the RHS of string
   implicit none
   character(*),intent(in)          :: strin
   character(1),intent(in),optional :: symbol
   character(:),allocatable         :: strout
   integer*4                        :: l
   character(1)                     :: sym
   if (present(symbol)) then
      sym = symbol
   else
      sym = '/'
   end if
   l = len(trim(strin))
   if (strin(l:l)==sym) then
      strout = trim(strin(1:l-1))
   else
      strout = trim(strin)
   end if
end function noslash

subroutine nil(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20)
   ! call this routine with unused function arguments to avoid compiler warnings
   class(*),optional :: x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20
   if (.false.) then
      select type(x1);  end select;    select type(x2);  end select
      select type(x3);  end select;    select type(x4);  end select
      select type(x5);  end select;    select type(x6);  end select
      select type(x7);  end select;    select type(x8);  end select
      select type(x9);  end select;    select type(x10); end select
      select type(x11); end select;    select type(x12); end select
      select type(x13); end select;    select type(x14); end select
      select type(x15); end select;    select type(x16); end select
      select type(x17); end select;    select type(x18); end select
      select type(x19); end select;    select type(x20); end select
   end if
end subroutine nil

subroutine check

end subroutine check

end module shared_module_system