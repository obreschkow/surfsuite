! analyses the format
! ./program [task_name] [task_value] [-option_name option_value] [-option_name option_value] ...
! where parentheses are optional
! and produces output on screen or logfile

module module_taskhandler

   public
   
   logical              :: stop_at_warnings = .false.
   logical              :: stop_at_errors = .true.
   
   integer*4            :: logfile_unit = 9 ! file unit for log-file
   character(len=255)   :: logfile_name = ''
   logical              :: logfile_open

   character(len=255)   :: task_name
   character(len=255)   :: task_value
   logical              :: task_has_value
   logical              :: task_exists
   
   integer*4,parameter  :: n_options_max = 10
   integer*4            :: n_options
   character(len=255)   :: option_name(n_options_max)
   character(len=255)   :: option_value(n_options_max)
   logical              :: option_used(n_options_max)
   
   contains
   
   subroutine get_arguments
   
      implicit none
      
      integer*4         :: narg
      character(255)    :: argument
      integer*4         :: i,j
      
      ! default values
      task_name = ''
      task_value = ''
      task_has_value = .false.
      task_exists = .false.
      n_options = 0
      option_name = ''
      option_value = ''
      option_used = .false.
      
      ! get number of arguments
      narg = iargc()
   
      if (narg>0) then
      
         ! handle task
         call getarg(1,argument)
         if (argument(1:1)=='-') then
            i = 1
         else
            task_exists = .true.
            task_name = argument
            if (narg>1) then
               call getarg(2,argument)
               if (argument(1:1)=='-') then
                  i = 2
               else
                  task_has_value = .true.
                  task_value = argument
                  i = 3
               end if
            else
               i = 2
            end if
         end if
         
         ! handle options
         if ((i<=narg).and.(mod(narg-i,2).ne.1)) call error('each option must have exactly one argument.')
         do while (i<=narg)
            n_options = n_options+1
            if (n_options>n_options_max) call error('too many options.')
            call getarg(i,argument)
            if (argument(1:1).ne.'-') then
               call error('option starting with "-" expected instead of "'//trim(argument)//'".')
            end if
            option_name(n_options) = argument
            call getarg(i+1,argument)
            if (argument(1:1)=='-') then
               call error('option "'//option_name(n_options)//'" must have a value not starting with "-".')
            end if
            option_value(n_options) = argument
            i = i+2
         end do
         if (i.ne.(narg+1)) then
            call error('unknown error in task handler.')
         end if
         
         ! check if any options appear multiple times
         do i = 1,n_options-1
            do j = i+1,n_options
               if (trim(option_name(i))==trim(option_name(j))) then
                  call error('option '//trim(option_name(i))//' appears multiple times.')
               end if
            end do
         end do
         
      end if
   
   end subroutine get_arguments
   
   subroutine using_option(i)
   
      implicit none
      integer*4,intent(in) :: i
      
      if (option_used(i)) then
         call warning('option '//trim(option_name(i))//' interpreted multiple times (developper issue?).')
      else
         option_used(i) = .true.
      end if
   
   end subroutine using_option
   
   subroutine require_task_value(reqired)
   
      implicit none
      logical,intent(in)   :: reqired
      if (reqired.and.(.not.task_has_value)) then
         call error('task '//trim(task_name)//' requires an argument.')
      else if ((.not.reqired).and.task_has_value) then
         call error('task '//trim(task_name)//' requires no argument.')
      end if
      
   end subroutine require_task_value
   
   subroutine require_no_options_left
   
      implicit none
      integer*4   :: i
      
      do i = 1,n_options
         if (.not.option_used(i)) then
            call error('option '//trim(option_name(i))//' unknown or not used for specific task.')
         end if
      end do
      
   end subroutine require_no_options_left
   
   subroutine out_start
   
      implicit none
      
      if (len(trim(logfile_name))>0) then
         open(logfile_unit,file=trim(logfile_name),action='write',status='replace',form='formatted')
         close(logfile_unit)
         logfile_open = .true.
      end if
      
   end subroutine out_start

   subroutine out(txt,i)
   
      implicit none
      character(*),intent(in)       :: txt
      integer*8,intent(in),optional :: i
      
      if (logfile_open) then
         open(logfile_unit,file=trim(logfile_name),action='write',status='old',position='append',form='formatted')
         if (present(i)) then
            write(logfile_unit,'(A,I0)') trim(txt)//' ',i
         else
            write(logfile_unit,'(A)') trim(txt)
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
   
   subroutine error(txt)
   
      implicit none
      character(*),intent(in)       :: txt
      
      call out('ERROR: '//trim(txt))
      if (stop_at_errors) stop
   
   end subroutine error
   
   subroutine warning(txt)
   
      implicit none
      character(*),intent(in)       :: txt
      
      call out('WARNING: '//trim(txt))
      if (stop_at_warnings) stop
   
   end subroutine warning

end module module_taskhandler