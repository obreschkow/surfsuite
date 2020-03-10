program surfsuite

   use module_taskhandler
   use module_system
   use module_io
   use module_sortparticles
   use module_getparticle
   use module_makehalos
   use module_gethalo
   use module_showhalo
   use module_trackhalo

   implicit none

   integer*4   :: forced_snapshot
   integer*4   :: i
   
   ! Change default options
   para%parameterfile = 'parameters.txt'
   para%simulation = ''
   forced_snapshot = -1
   
   ! handle arguments
   call get_arguments
   
   if (.not.task_exists) then
      call out('The general use is: surfsuite task [-option ...]')
      call out('Consult the README file for additional information.')
      stop
   end if
   
   do i = 1,n_options
   
      select case (trim(option_name(i)))
      case ('-parameterfile')
         call using_option(i)
         para%parameterfile = trim(option_value(i))
      case ('-simulation')
         call using_option(i)
         para%simulation = trim(option_value(i))
      case ('-snapshot')
         call using_option(i)
         read(option_value(i),*) forced_snapshot
      case ('-logfile')
         call using_option(i)
         logfile_name = trim(option_value(i))
      end select
   
   end do
   
   if (trim(task_name)=='version') then
      call require_task_value(.false.)
      call require_no_options_left
      write(*,'(A,A,A)') 'This is SurfSuite Version ',version,'.'
      write(*,'(A)') 'Consult the README file for additional information.'
      stop
   end if
   
   ! initialize logfile   
   call out_start
   
   ! load parameters
   call load_parameters(forced_snapshot)

   ! TASKS
   select case (trim(task_name))
   case ('simulation')
      call require_task_value(.false.)
      call require_no_options_left
      call task_getgadgetproperties
   case ('sortparticles')
      call require_task_value(.false.)
      call require_no_options_left
      call task_sortparticles
   case ('makehalos')
      call require_task_value(.false.)
      call require_no_options_left
      call task_makehalos
   case ('makeall')
      call require_task_value(.false.)
      call require_no_options_left
      call task_sortparticles
      call task_makehalos
   case ('getparticle')
      call require_task_value(.true.)
      call task_getparticle
   case ('gethalo')
      call require_task_value(.true.)
      call task_gethalo
   case ('showhalo')
      call require_task_value(.true.)
      call task_showhalo
   case ('trackhalo')
      call require_task_value(.true.)
      call task_trackhalo
   case default
      call error('"'//trim(task_name)//'" is an unknown task.')
   end select
    
end program surfsuite