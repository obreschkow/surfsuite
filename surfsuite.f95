program surfsuite

   use shared_module_interface
   use shared_module_conversion
   use module_io
   use module_sortparticles
   use module_getparticle
   use module_makehalos
   use module_gethalo
   use module_showhalo
   use module_trackhalo

   implicit none
   
   ! start user interface
   call set_version_text('This is surfsuite version '//trim(version)//'.')
   call interface_start(require_task=.true.)
   
   ! handle general options
   call get_option_value(para%parameterfile,'-parameterfile','parameters.txt')
   call get_option_value(para%simulation,'-simulation','')
   
   ! load parameters
   call load_parameters
   
   ! overwrite default snapshot, if option -snapshot given
   call get_option_value(para%snapshot,'-snapshot')
   
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
   call interface_stop
    
end program surfsuite