program surfsuite

   use shared_module_core
   use shared_module_arguments
   use shared_module_parameters
   use module_io
   use module_sortparticles
   use module_getparticle
   use module_makehalos
   use module_gethalo
   use module_showhalo
   use module_trackhalo

   implicit none
   
   ! start user interface
   call set_version('0.34')
   call handle_arguments(require_task=.true.)
   call start_output
   
   ! handle general options
   call get_option_value(para%parameterfile,'-parameterfile','parameters.txt')
   call set_parameterfile(para%parameterfile)
   call get_option_value(para%parameterset,'-parameterset','')
   call set_parameterset(para%parameterset)
   
   ! load parameters
   call load_parameters
   para%parameterset = parameterset ! this is in case of a default parameterset
   
   ! overwrite default snapshot, if option -snapshot given
   call get_option_value(para%snapshot,'-snapshot',int(para%snapshot,4)) ! int4-conversion helps avoid warning of in=out
   
   ! run tasks
   if (istask('simulation',require_value=.false.,require_options=.false.)) then
      call task_getgadgetproperties
   else if (istask('sortparticles',.false.,.false.)) then
      call task_sortparticles
   else if (istask('makehalos',.false.,.false.)) then
      call task_makehalos
   else if (istask('makeall',.false.,.false.)) then
      call task_sortparticles
      call task_makehalos
   else if (istask('getparticle',.true.,.false.)) then
      call task_getparticle
   else if (istask('gethalo',.true.)) then
      call task_gethalo
   else if (istask('showhalo',.true.)) then
      call task_showhalo
   else if (istask('trackhalo',.true.)) then
      call task_trackhalo
   else
      call unknown_task
   end if
   
   ! finalize output on screen/logfile
   call stop_output
    
end program surfsuite