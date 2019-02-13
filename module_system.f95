module module_system

use module_global

! computation time evaluation
integer*8               :: tstart_total
integer*8               :: tstart
integer*8               :: trate

! screen output
character(len=255)      :: logfilename
logical                 :: opt_logfile

contains

subroutine hline
   implicit none
   call out('----------------------------------------------------------------------------------')
end subroutine hline

subroutine out_open
   implicit none
   if (opt_logfile) then
      open(9,file=trim(logfilename),action='write',status='replace',form='formatted')
      write(9,'(A)') '----------------------------------------------------------------------------------'
      write(9,'(A)') 'Logfile SurfSuite Version '//version
      close(9)
   end if
end subroutine out_open

subroutine out(txt,i)
   implicit none
   character(*),intent(in)       :: txt
   integer*8,intent(in),optional :: i
   if (opt_logfile) then
      open(9,file=trim(logfilename),action='write',status='old',position='append',form='formatted')
      if (present(i)) then
         write(9,'(A,I0)') trim(txt)//' ',i
      else
         write(9,'(A)') trim(txt)
      end if
      close(9)
   else
      if (present(i)) then
         write(*,'(A,I0)') trim(txt)//' ',i
      else
         write(*,'(A)') trim(txt)
      end if
   end if
end subroutine out

subroutine tic
   implicit none
   call system_clock(tstart,trate)
end subroutine tic  

subroutine toc
   implicit none
   integer*8 :: tstop
   call system_clock(tstop)
   call out('Time taken: '//trim(time_string(real(tstop-tstart,8)/trate)))
   call hline
end subroutine toc

subroutine tic_total
   implicit none
   call system_clock(tstart_total,trate)
   call hline
end subroutine tic_total

subroutine toc_total
   implicit none
   integer*8 :: tstop
   call system_clock(tstop)
   call out('TOTAL TIME TAKEN: '//trim(time_string(real(tstop-tstart_total,8)/trate)))
   call hline
end subroutine toc_total

function time_string(secs) result(str)
   implicit none
   real*8,intent(in)          :: secs
   real*4                     :: seconds
   integer*4                  :: minutes,hours,days
   character(100)             :: str
   character(1)               :: zero
   minutes = int(secs/60,4)
   hours = int(secs/3600,4)
   days = int(secs/86400,4)
   seconds = real(secs-minutes*60,4)
   minutes = minutes-hours*60
   hours = hours-days*24
   if (seconds<1) then
      zero = '0'
   else
      zero = ''
   end if
   if (days>0) then
      write(str,'(I0,A,I0,A,I0,A,F0.2,A)') days,'d ',hours,'h ', minutes,'m '//trim(zero),seconds,'s'
   else if (hours>0) then
      write(str,'(I0,A,I0,A,F0.2,A)') hours,'h ',minutes,'m '//trim(zero),seconds,'s'
   else if (minutes>0) then
      write(str,'(I0,A,F0.2,A)') minutes,'m '//trim(zero),seconds,'s'
   else
      write(str,'(A,F0.2,A)') trim(zero),seconds,'s'
   end if
end function time_string

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

subroutine eigen(A,values,vectors)

   ! returns the eigenvalues (from smallest to largest)
   ! and optionally the eigenvectors vectors(:,1), vectors(:,2), ...
   ! requires LAPACK
   
   implicit none
   real*4,intent(in)             :: A(:,:)   ! symmetric NxN matrix
   real*4,intent(out)            :: values(:)
   real*4,intent(out),optional   :: vectors(:,:)
   real*4,allocatable            :: W(:)     ! eigenvalues
   real*4,allocatable            :: S(:,:)
   integer*4                     :: N        ! order of the matrix
   integer*4                     :: lwork,info
   integer*4,parameter           :: lwmax = 1000
   real*4                        :: work(lwmax)
   
   allocate(S(size(A,1),size(A,2)))
   S = A
   N = size(S,1)
   allocate(W(N))
   
   ! Query the optimal workspace.
   lwork = -1
   CALL sSYEV( 'Vectors', 'Upper', N, S, N, W, work, lwork, info)
   lwork = min(lwmax,int(work(1),4))
   
   ! Solve eigenproblem.
   CALL sSYEV( 'Vectors', 'Upper', N, S, N, W, work, lwork, info)
   
   ! Return results
   values = W
   if (present(vectors)) vectors = S
   
end subroutine eigen

end module module_system