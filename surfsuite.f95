program surfsuite

   use module_interface
   use module_system
   use module_io
   use module_sortparticles
   use module_getparticle
   use module_makehalos
   use module_gethalo
   use module_showhalo
   use module_trackhalo

   implicit none

   ! Change default options
   para%parameterfile = 'parameters.txt'
   para%simulation = ''
   
   ! start user interface
   call set_version_text('This is surfsuite version '//trim(version)//'.')
   call interface_start
   call require_task(.true.) ! this is to say that the programm always requires a task (unless the option is -version/-v)
   
   ! handle options not dealt with by the interface (e.g. all except -version, -verbose, -logfile)
   if (opt('-parameterfile')) para%parameterfile = trim(opt_val)
   if (opt('-simulation')) para%simulation = trim(opt_val)
   
   ! load parameters
   call load_parameters
   
   ! overwrite default snapshot, if requested by user
   if (opt('-snapshot')) read(opt_val,*) para%snapshot

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
      call require_no_options_left
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
   
   ! finalize output on screen/logfile
   call interface_stop
    
end program surfsuite