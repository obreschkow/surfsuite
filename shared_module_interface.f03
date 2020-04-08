! This module can be used to handle the user interface of Fortran programs
!
! 1) It extracts, handles and checks the input arguments in a program call of the form
! ./program [task_name [task_value]] [-option_name option_value] [-option_name option_value] ...
! where parentheses are optional.
!
! 2) It handles programm outputs on the screen or logfile, including timings, errors, warnings, progress, lines
!
! 3) It can produce warnings for files that do not exist

module shared_module_interface

   private
   
   ! argument handling routines
   public   :: set_version_text ! sets text to be displayed when version is requested (e.g. using -version)
   public   :: interface_start ! analyses arguments, initialises screen/logfile output
   public   :: interface_stop ! finalises screen/logfile output
   public   :: require_task ! ensures that a task does or does not exist (depending on arguemnt)
   public   :: istask ! logical function checking if the task_name is equal to the argument; can also check existence of task_value and options
   public   :: get_task_value ! subroutine returning the task value (as output argument, not as a function)
   public   :: get_option_value ! function returning option value (see function description for further details)
   public   :: require_no_options_left ! check if get_option_value has been used on all available options, if not produce error
   public   :: unknown_task ! display error message for unknown task_name
   
   ! screen/logfile output
   public   :: out ! genering output routine
   public   :: error ! error message and stop
   public   :: warning ! warning message
   public   :: hline ! horizontal line
   public   :: progress ! output progress in percent
   public   :: tic ! draw horizontal line and start timer (normally used at the beginning of a program section)
   public   :: toc ! output wall time since last call of tic and draw horizontal line (used at the end of a program section)
   
   ! file check
   public   :: checkfile ! check if file exists; if not, produce error message and stop
   
   ! less frequently used
   public   :: task_exists ! read-only logical flag specifying if a task name is given; normally require_task is used instead
   public   :: task_name ! read-only string of the task name; normally function istask() is used instead
   public   :: task_has_value ! read-only logical flag specifying if the task has a value; normally this is checked with istask()
   public   :: task_value
   public   :: noptions ! read-only integer giving the number of optional arguments provided by the user
   public   :: verbose ! read-only logical flag specifying if an output is displayed on screen/logfile; normally specified using -verbose
   public   :: set_verbose ! routine to set verbose programmatically
   public   :: logfile_name ! read-only string of the logfile name; normally specified using -logfile
   public   :: set_logfile ! routine to set logfile_name programmatically
   public   :: logfile_open ! read-only logical flag specifying if the logfile has been initialised
   public   :: logfile_unit ! read-only integer giving the I/O-unit used for the logfile
   public   :: time_rate ! read-only integer*8 containing the time_rate of the system clock
   
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
   character(len=255),private    :: version_str = 'Version not specified.'
   logical,protected             :: just_made_hline = .false.
   logical,protected             :: once_made_hline = .false.
   
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
   
   interface get_option_value
      module procedure get_option_value_string
      module procedure get_option_value_int4
      module procedure get_option_value_int8
      module procedure get_option_value_real4
      module procedure get_option_value_real8
      module procedure get_option_value_logical
   end interface get_option_value
   
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
         call hline
         call out(version_str)
         call out('Developed by Danail Obreschkow (danail.obreschkow@icrar.org).')
         call out('Consult the README file for additional information.')
         call hline
         stop
      end if
   end if
   
   ! extract arguments and split into tasks and options
   call get_arguments
   
   ! start output on screen/logfile, as specified by options -verbose (default 1) and -logfile (default '' = screen)
   call get_option_value(verbose,'-verbose',.true.)
   call get_option_value(logfile_name,'-logfile','')
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

subroutine out(txt,value)

   ! write text on screen / in logfile; optional number of type int8 can be added

   implicit none
   character(*),intent(in)       :: txt
   class(*),intent(in),optional  :: value
   character(len=255)            :: string
   
   if (verbose) then
   
      if (present(value)) then
         write(string,'(A,A)') txt,val2str(value)
      else
         write(string,'(A)') txt
         just_made_hline = trim(txt)==hline_str
      end if

      if (logfile_open) then
         open(logfile_unit,file=trim(logfile_name),action='write',status='old',position='append',form='formatted')
         write(logfile_unit,'(A)') trim(string)
         close(logfile_unit)
      else
         write(*,'(A)') trim(string)
      end if
      
   end if
         
end subroutine out

subroutine hline

   ! outputs a horizontal line, unless such a line has just been written

   implicit none
   if (.not.just_made_hline) call out(hline_str)
   once_made_hline = .true.
   
end subroutine hline

subroutine error(txt)

   ! write error message and stop code

   implicit none
   character(*),intent(in)       :: txt
   
   if (.not.once_made_hline) call hline
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

subroutine progress(fraction)

   ! write progress in percent

   implicit none
   real*4,intent(in) :: fraction
   character(len=20) :: str
   
   write(str,'(A,F6.2,A)') 'Progress: ',fraction*100,'%'
   call out(trim(str))

end subroutine progress


! handle checks *****************************************************************************************************************   

subroutine checkfile(filename)

   ! check if file exists, if not produce error
   implicit none
   character(*),intent(in)   :: filename
   logical                   :: res
   inquire(file=trim(filename), exist=res)
   if (.not.res) call error('file does not exist: '//trim(filename))
   
end subroutine checkfile


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

logical function istask(name,require_value,require_options)

   ! check if task_name is equal to name and optionally check for the existence of values and options
   ! if require_value is not given, the task can have a value or not
   ! ir require_options is not given *or set to true*, the task can have options or not -> use get_option_values to require options
   
   implicit none
   character(*),intent(in)       :: name
   logical,intent(in),optional   :: require_value ! if set, the task must have a value (true) or *must not* have a value (false)
   logical,intent(in),optional   :: require_options ! if false, the task *must not* have options; true is ignored -> use get_option_values
   logical                       :: options_left
   integer*4                     :: i
   
   if (task_exists) then
   
      istask = trim(name)==trim(task_name)
   
      if (istask) then
         if (present(require_value)) then
            if (require_value.and.(.not.task_has_value)) call error('task '//trim(task_name)//' requires an argument.')
            if ((.not.require_value).and.task_has_value) call error('task '//trim(task_name)//' requires no argument.')
         end if
         if (present(require_options)) then
            options_left = .false.
            do i = 1,n_options
               options_left = options_left.or.(.not.option_used(i))
            end do
            if ((.not.require_options).and.options_left) call error('task '//trim(task_name)//' requires no options.')
         end if
      end if
      
   else
   
      istask = .false.
   
   end if

end function istask

subroutine unknown_task

   implicit none
   if (.not.task_exists) call error('@developer: do not call unknown_task if task does not exist')
   call error('"'//trim(task_name)//'" is an unknown task.')
   
end subroutine unknown_task

subroutine get_task_value(value)

   use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
   
   implicit none
   class(*),intent(out) :: value
   
   if (.not.task_has_value) call error('@developer: do not call get_task_value if no task_value exists; '//&
   & 'use istask(...,require_value=.false.)')
   
   select type (value)
   type is (integer(kind=int8));    read(task_value,*) value
   type is (integer(kind=int16));   read(task_value,*) value
   type is (integer(kind=int32));   read(task_value,*) value
   type is (integer(kind=int64));   read(task_value,*) value
   type is (real(kind=real32));     read(task_value,*) value
   type is (real(kind=real64));     read(task_value,*) value
   type is (real(kind=real128));    read(task_value,*) value
   type is (logical);               read(task_value,*) value
   type is (character(*));          read(task_value,*) value
   type is (complex(kind=real32));  read(task_value,*) value
   type is (complex(kind=real64));  read(task_value,*) value
   type is (complex(kind=real128)); read(task_value,*) value
   class default
      call error('@developer: unknown variable type')
   end select
      
end subroutine get_task_value

subroutine require_no_options_left

   ! check if all options have been used and produce an error if not

   implicit none
   integer*4   :: i
   
   do i = 1,n_options
      if (.not.option_used(i)) call error('option '//trim(option_name(i))//' unknown or not used for specific task.')
   end do
   
end subroutine require_no_options_left

subroutine get_option_value_string(value,name,preset,required)
   implicit none
   character(*),intent(inout)       :: value
   character(*),intent(in)          :: name     ! name of option
   character(*),intent(in),optional :: preset   ! default value
   logical,intent(in),optional      :: required ! if given and true, the option must be given by the user
   integer*4                        :: i
   i = option_index(name,required)
   if (i==0) then
      if (present(preset)) value = preset
   else
      value = trim(option_value(i))
   end if
end subroutine get_option_value_string

subroutine get_option_value_int4(value,name,preset,min,max,required)
   implicit none
   integer*4,intent(inout)       :: value
   character(*),intent(in)       :: name     ! name of option
   integer*4,intent(in),optional :: preset   ! default value
   integer*4,intent(in),optional :: min,max  ! optional range required for value
   logical,intent(in),optional   :: required ! if given and true, the option must be given by the user
   integer*4                     :: i
   i = option_index(name,required)
   if (i==0) then
      if (present(preset)) value = preset
   else
      read(option_value(i),*) value
   end if
   if (present(min)) then
      if (value<min) call error('option '//trim(name)//' cannot be smaller than '//val2str(min))
   end if
   if (present(max)) then
      if (value>max) call error('option '//trim(name)//' cannot be larger than '//val2str(max))
   end if
end subroutine get_option_value_int4

subroutine get_option_value_int8(value,name,preset,min,max,required)
   implicit none
   integer*8,intent(inout)       :: value
   character(*),intent(in)       :: name     ! name of option
   integer*8,intent(in),optional :: preset   ! default value
   integer*8,intent(in),optional :: min,max  ! optional range required for value
   logical,intent(in),optional   :: required ! if given and true, the option must be given by the user
   integer*4                     :: i
   i = option_index(name,required)
   if (i==0) then
      if (present(preset)) value = preset
   else
      read(option_value(i),*) value
   end if
   if (present(min)) then
      if (value<min) call error('option '//trim(name)//' cannot be smaller than '//val2str(min))
   end if
   if (present(max)) then
      if (value>max) call error('option '//trim(name)//' cannot be larger than '//val2str(max))
   end if
end subroutine get_option_value_int8

subroutine get_option_value_real4(value,name,preset,min,max,required)
   implicit none
   real*4,intent(inout)          :: value
   character(*),intent(in)       :: name     ! name of option
   real*4,intent(in),optional    :: preset   ! default value
   real*4,intent(in),optional    :: min,max  ! optional range required for value
   logical,intent(in),optional   :: required ! if given and true, the option must be given by the user
   integer*4                     :: i
   i = option_index(name,required)
   if (i==0) then
      if (present(preset)) value = preset
   else
      read(option_value(i),*) value
   end if
   if (present(min)) then
      if (value<min) call error('option '//trim(name)//' cannot be smaller than '//val2str(min))
   end if
   if (present(max)) then
      if (value>max) call error('option '//trim(name)//' cannot be larger than '//val2str(max))
   end if
end subroutine get_option_value_real4

subroutine get_option_value_real8(value,name,preset,min,max,required)
   implicit none
   real*8,intent(inout)          :: value
   character(*),intent(in)       :: name     ! name of option
   real*8,intent(in),optional    :: preset   ! default value
   real*8,intent(in),optional    :: min,max  ! optional range required for value
   logical,intent(in),optional   :: required ! if given and true, the option must be given by the user
   integer*4                     :: i
   i = option_index(name,required)
   if (i==0) then
      if (present(preset)) value = preset
   else
      read(option_value(i),*) value
   end if
   if (present(min)) then
      if (value<min) call error('option '//trim(name)//' cannot be smaller than '//val2str(min))
   end if
   if (present(max)) then
      if (value>max) call error('option '//trim(name)//' cannot be larger than '//val2str(max))
   end if
end subroutine get_option_value_real8

subroutine get_option_value_logical(value,name,preset,required)
   implicit none
   logical,intent(inout)         :: value
   character(*),intent(in)       :: name     ! name of option
   logical,intent(in),optional   :: preset   ! default value
   logical,intent(in),optional   :: required ! if given and true, the option must be given by the user
   integer*4                     :: i
   i = option_index(name,required)
   if (i==0) then
      if (present(preset)) value = preset
   else
      if (any(trim(option_value(i))==(/'0','F','f','N','n'/))) then
         value = .false.
      else if (any(trim(option_value(i))==(/'1','T','t','Y','y'/))) then
         value = .true.
      else
         call error('option '//trim(name)//' only takes logical arguments (0/f/F/n/N and 1/t/T/y/Y).')
      end if
   end if
end subroutine get_option_value_logical

integer*4 function option_index(name,required)

   ! check of option exists, and if so, write it's value into opt_val

   implicit none
   character(*),intent(in)       :: name ! name of option
   logical,intent(in),optional   :: required
   integer*4                     :: i
   
   option_index = 0
   do i = 1,n_options
      if (trim(option_name(i))==trim(name)) then
         option_index = i
         exit
      end if
   end do
   
   if (option_index>0) option_used(option_index)=.true.
   
   if (present(required)) then
      if (required.and.(option_index==0)) call error('argument '//trim(name)//' must be specified.')
   end if
   
end function option_index

function val2str(value) result(str)

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
      call error('@developer: unknown variable type')
   end select

   str = trim(txt)

end function val2str

end module shared_module_interface