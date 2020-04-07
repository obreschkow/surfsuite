! This module can be used to handle the user interface of Fortran programs
!
! 1) It extracts, handles and checks the input arguments in a program call of the form
! ./program [task_name [task_value]] [-option_name option_value] [-option_name option_value] ...
! where parentheses are optional.
!
! 2) It handles programm outputs on the scree or logfile, including timings

module shared_module_interface

   public
   
   ! all variables declared here are read-only for other modules (the parameters are also read-only for this module)
   
   ! handling timing
   integer*8,protected           :: time_start = -1
   integer*8,protected           :: time_tic = -1
   integer*8,protected           :: time_rate = -1
   
   ! handling screen/logfile outputs
   logical,parameter             :: stop_at_warnings = .false.
   logical,protected             :: verbose = .true. ! can be via option -verbose or programmatically using set_verbose
   integer*4,parameter           :: logfile_unit = 999 ! file unit for log-file
   character(len=255),protected  :: logfile_name = '' ! a logfile is produced if this is specified, can be set via option -logfile or programmatically using set_logfile
   logical,protected             :: logfile_open = .false.
   
   character(*),parameter        :: hline_str = '----------------------------------------------------------------------------------'
   character(len=255),protected  :: version_str = 'Version not specified.'
   logical                       :: just_made_hline = .false.
   
   ! handling arguments
   character(len=255),protected  :: task_name
   character(len=255),protected  :: task_value
   logical,protected             :: task_has_value
   logical,protected             :: task_exists
   
   integer*4,parameter           :: n_options_max = 10
   integer*4,protected           :: n_options
   character(len=255),protected  :: option_name(n_options_max)
   character(len=255),protected  :: option_value(n_options_max)
   logical,protected             :: option_used(n_options_max)
   character(len=255),protected  :: opt_val  ! value of the option that was last queried using the function opt()
   
   contains
   
   ! master routines ***************************************************************************************************************
   
   subroutine interface_start
   
      implicit none
      integer*4         :: narg
      character(255)    :: argument
      
      ! return version text, if the first argument is version/-version/-v/help/-help
      narg = iargc() ! get number of arguments
      if (narg>0) then
         call getarg(1,argument)
         if ((argument=='version').or.(argument=='-version').or.(argument=='-v').or.(argument=='-help').or.(argument=='help')) then
            write(*,'(A)') hline_str
            write(*,'(A)') trim(version_str)
            write(*,'(A)') 'Developed by Danail Obreschkow (danail.obreschkow@icrar.org).'
            write(*,'(A)') 'Consult the README file for additional information.'
            write(*,'(A)') hline_str
            stop
         end if
      end if
      
      ! extract arguments and split into tasks and options
      call get_arguments
      
      ! start output on screen/logfile, as specified by options -verbose (default 1) and -logfile (default '' = screen)
      if (opt('-verbose')) verbose = trim(opt_val)=='1'
      if (opt('-logfile')) logfile_name = trim(opt_val)
      if (trim(logfile_name).ne.'') then
         open(logfile_unit,file=trim(logfile_name),action='write',status='replace',form='formatted')
         close(logfile_unit)
         logfile_open = .true.
      end if
      
      ! output horizontal line
      call hline
      
      ! start timer
      call system_clock(time_start,time_rate)
   
   end subroutine interface_start
   
   subroutine interface_stop
   
      ! close output on screen/logfile and write total time taken
   
      implicit none
      integer*8 :: time_close
      
      if ((time_start==-1).or.(time_rate==1)) call error('@developer: timer must be initialized via interface_start.')
      
      call system_clock(time_close)
      
      call hline
      call out('TOTAL WALL TIME: '//trim(time_string(real(time_close-time_start,8)/time_rate)))
      call hline
   
   end subroutine interface_stop
   
   subroutine set_version_text(txt)
   
      implicit none
      character(*),intent(in) :: txt
      version_str = trim(txt)
      
   end subroutine set_version_text
   
   
   ! timing routines ***************************************************************************************************************
   
   function time_string(secs) result(str)
   
      ! convertes seconds into human-readable text format
   
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
   
   subroutine tic(title)
   
      ! start timer for program part, denoted with a horizontal line in the output
      
      implicit none
      character(*),optional,intent(in) :: title
      
      call hline
      if (present(title)) call out(trim(title))
      
      if (time_rate==-1) then
         call system_clock(time_tic,time_rate)
      else
         call system_clock(time_tic)
      end if
      
   end subroutine tic  

   subroutine toc
   
      ! stop timer for program part, denoted with a horizontal line in the output
   
      implicit none
      integer*8 :: toc_time
      
      if ((time_tic==-1).or.(time_rate==1)) call error('@developer: timer must be initialized via tic.')
      
      call system_clock(toc_time)
      call out('Wall time: '//trim(time_string(real(toc_time-time_tic,8)/time_rate)))
      call hline
      
   end subroutine toc
   
   
   ! handle scree/logfile output ***************************************************************************************************
   
   subroutine set_verbose(value)
   
      implicit none
      logical,intent(in) :: value
      verbose = value
   
   end subroutine set_verbose
   
   subroutine set_logfile(txt)
   
      implicit none
      character(*),intent(in) :: txt
      logfile_name = txt
   
   end subroutine set_logfile

   subroutine out(txt,i)
   
      ! write text on screen / in logfile; optional number of type int8 can be added
   
      implicit none
      character(*),intent(in)       :: txt
      integer*8,intent(in),optional :: i
      logical                       :: suppress_out
      
      if (verbose) then
      
         if (trim(txt)==hline_str) then ! avoids having two consecutive horizontal lines
            if (just_made_hline) then
               suppress_out = .true.
            else
               just_made_hline = .true.
               suppress_out = .false.
            end if
         else
            just_made_hline = .false.
            suppress_out = .false.
         end if
      
         if (.not.suppress_out) then
      
            if (logfile_open) then
               open(logfile_unit,file=trim(logfile_name),action='write',status='old',position='append',form='formatted')
               if (present(i)) then
                  write(logfile_unit,'(A,I0)') trim(txt)//' ',i
               else
                  write(logfile_unit,'(A)') trim(txt)
               end if
               close(logfile_unit)
            else
               if (present(i)) then
                  write(*,'(A,I0)') trim(txt)//' ',i
               else
                  write(*,'(A)') trim(txt)
               end if
            end if
            
         end if
         
      end if
            
   end subroutine out
   
   subroutine hline
   
      ! outputs a horizontal line, unless such a line has just been written
   
      implicit none
      call out(hline_str)
      
   end subroutine hline
   
   subroutine error(txt)
   
      ! write error message and stop code
   
      implicit none
      character(*),intent(in)       :: txt
      
      call out('ERROR: '//trim(txt))
      call hline
      stop
      
   end subroutine error
   
   subroutine warning(txt)
   
      ! write warning; the code is stopped if stop_at_warnings==.true.
   
      implicit none
      character(*),intent(in)       :: txt
      
      call out('WARNING: '//trim(txt))
      if (stop_at_warnings) then
         call hline
         stop
      end if
      
   end subroutine warning
   
   
   ! handle input arguments ********************************************************************************************************
   
   subroutine get_arguments
   
      ! extract all the user arguments and group them into the global variables
      ! task_name, task_value, option_name(:), option_value(:)
   
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
            if (argument(1:1).ne.'-') call error('option starting with "-" expected instead of "'//trim(argument)//'".')
            option_name(n_options) = argument
            call getarg(i+1,argument)
            if (argument(1:1)=='-') call error('option "'//option_name(n_options)//'" must have a value not starting with "-".')
            option_value(n_options) = argument
            i = i+2
         end do
         if (i.ne.(narg+1)) call error('@developer: unknown error in task handler.')
         
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
   
   subroutine require_task(required)
   
      ! checks if task_name does or does not exists, as required
   
      implicit none
      logical,intent(in) :: required
      if (required.and.(.not.task_exists)) call error('Task is missing. Consult README file for additional information.')
      if ((.not.required).and.task_exists) call error('Unknown argument '//trim(task_name))
      
   end subroutine require_task
   
   subroutine require_task_value(required)
   
      ! check if task_value does or does not exist, as required
   
      implicit none
      logical,intent(in) :: required
      
      if (.not.task_exists) call error('@developer: do not call require_task_value if task does not exist')
      if (required.and.(.not.task_has_value)) call error('task '//trim(task_name)//' requires an argument.')
      if ((.not.required).and.task_has_value) call error('task '//trim(task_name)//' requires no argument.')
      
   end subroutine require_task_value
   
   logical function opt(name,required,allow_multiple_use)
   
      ! check of option exists, and if so, write it's value into opt_val
   
      implicit none
      character(*),intent(in)       :: name ! name of option
      integer*4                     :: i
      logical,intent(in),optional   :: required
      logical,intent(in),optional   :: allow_multiple_use
      
      opt = .false.
      opt_val = ''
      do i = 1,n_options
         if (trim(option_name(i))==trim(name)) then
            opt_val = option_value(i)
            opt = .true.
            call using_option(i,allow_multiple_use=allow_multiple_use)
         end if
      end do
      
      if (present(required)) then
         if (required.and.(.not.opt)) call error('argument '//trim(name)//' must be specified.')
      end if
   
   end function opt
   
   subroutine using_option(i,allow_multiple_use)
   
      ! memorize that option i has been used and output a warning if this option has been used multiple times
   
      implicit none
      integer*4,intent(in)          :: i
      logical,intent(in),optional   :: allow_multiple_use
      logical                       :: warn
      
      if (option_used(i)) then
         warn = .true.
         if (present(allow_multiple_use)) warn = .not.allow_multiple_use
         if (warn) call warning('@developer: option '//trim(option_name(i))//' interpreted multiple times.')
      end if
      
      option_used(i) = .true.
   
   end subroutine using_option
   
   subroutine require_no_options_left
   
      ! check if all options have been used and produce an error if not
   
      implicit none
      integer*4   :: i
      
      do i = 1,n_options
         if (.not.option_used(i)) call error('option '//trim(option_name(i))//' unknown or not used for specific task.')
      end do
      
   end subroutine require_no_options_left

end module shared_module_interface