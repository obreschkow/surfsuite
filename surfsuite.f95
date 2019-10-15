program surfsuite

   use module_system
   use module_io
   use module_sortparticles
   use module_getparticle
   use module_makehalos
   use module_gethalo
   use module_renderhalo
   use module_analysis

   implicit none

   character(len=255) :: arg_task
   character(len=255) :: arg_option
   character(len=255) :: arg_value
   character(len=255) :: forced_snapshot
   integer*4          :: i,val
   
   narg = iargc() ! number of arguments
   
   if (narg==0) then
      write(*,'(A)') 'The general use is: surfsuite task [-option ...]'
      write(*,'(A)') 'Consult the README file for additional information.'
      stop
   end if
   
   ! Change default options
   para%parameterfile = 'parameters.txt'
   para%simulation = ''
   forced_snapshot = ''
   opt_logfile = .false.
   if (narg>1) then
      do i = 2,narg-1
         call getarg(i,arg_option)
         call getarg(i+1,arg_value)
         select case (trim(arg_option))
         case ('-parameterfile')
            para%parameterfile = trim(arg_value)
         case ('-simulation')
            para%simulation = trim(arg_value)
         case ('-snapshot')
            if (len(trim(arg_value))<=3) then
               read(arg_value,*) val
               write(forced_snapshot,'(A,I0.3)') 'snapshot_',val
            else
               forced_snapshot = trim(arg_value)
            end if
         case ('-logfile')
            logfilename = trim(arg_value)
            opt_logfile = .true.
         end select
      end do
   end if
   
   call out_open
   
   call load_parameters(forced_snapshot)

   ! TASKS
   call getarg(1,arg_task)
   select case (trim(arg_task))
   case ('')
      call task_version
   case ('version')
      call task_version
   case ('-v')
      call task_version
   case ('-version')
      call task_version
   case ('simulation')
      call task_getgadgetproperties
   case ('sortparticles')
      call task_sortparticles
   case ('makehalos')
      call task_makehalos
   case ('makeall')
      call task_sortparticles
      call task_makehalos
   case ('getparticle')
      call task_getparticle
   case ('gethalo')
      call task_gethalo
   case ('renderhalo')
      call task_renderhalo
   case ('analysis')
      call task_analysis
   case default
      call out('ERROR: '//trim(arg_task)//' is an unknown task.')
      stop
   end select
   
   contains

   subroutine  task_version
      implicit none
      write(*,'(A,A,A)') 'This is SurfSuite Version ',version,'.'
      write(*,'(A)') 'Consult the README file for additional information.'
   end subroutine task_version
    
end program surfsuite