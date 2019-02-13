module module_io

use module_global
use module_system

contains

function exists(filename,do_not_stop) result(res)
   implicit none
   character(len=*),intent(in)   :: filename
   logical,intent(in),optional   :: do_not_stop
   logical                       :: res
   inquire(file=trim(filename), exist=res)
   if ((.not.res).and.(.not.present(do_not_stop))) then
      call out('ERROR: File does not exist: '//trim(filename))
      stop
   end if
end function exists

subroutine check_exists(filename)
   implicit none
   character(*),intent(in) :: filename
   if (.not.exists(filename)) stop
end subroutine check_exists

function filename(index,part1,part2,part3,multi) result(fn)

   ! returns the concatenated filename
   ! [part1][part2][part3].[index]
   ! if this file exists, otherwise (and only if index==0)
   ! [part1][part2][part3]
   ! if it exists
   ! the first or second format can be forced by setting multi

   implicit none
   integer,intent(in)               :: index
   character(*),intent(in)          :: part1
   character(*),intent(in),optional :: part2
   character(*),intent(in),optional :: part3
   logical,intent(in),optional      :: multi
   character(len=255)               :: filebase,filebasedot,fn
   character(len=5)                 :: str
   
   filebase = trim(part1)
   if (present(part2)) filebase = trim(filebase)//trim(part2)
   if (present(part3)) filebase = trim(filebase)//trim(part3)
   
   write(str,'(I5)') index
   filebasedot = trim(filebase)//'.'//adjustl(trim(str))
   
   if (present(multi)) then
      if (multi) then
         fn = trim(filebasedot)
      else
         if (index>0) then
            call out('Error in function filename(): single file with index>0')
            stop
         end if
         fn = trim(filebase)
      end if
   else
      if (exists(trim(filebase)//'.0',.true.).or.(index>0)) then
         fn = trim(filebasedot)
      else
         fn = trim(filebase)
      end if
   end if
   
end function filename

function get_number_of_subfiles(filebase) result(nfiles)

   ! counts the number of files with filebase, i.e. [filebase].0, [filebase].1, ...

   implicit none
   character(*),intent(in) :: filebase
   character(len=255)      :: fn
   logical                 :: single_file_exists
   logical                 :: multiple_file_exists
   integer*4               :: i,nfiles
   
   ! determine if the snapshot is stored in one or multiple files
   inquire(file=trim(filename(0,filebase,multi=.false.)),exist=single_file_exists)
   inquire(file=trim(filename(0,filebase,multi=.true.)),exist=multiple_file_exists)
   if ((.not.single_file_exists).and.(.not.multiple_file_exists)) then
      call out('Error: Could not find any files for the base')
      call out(trim(filebase))
      stop
   else if (single_file_exists.and.multiple_file_exists) then
      call out('Error: Ambiguous, since single and multiple files exist for the base')
      call out(trim(filebase))
      stop
   else if (single_file_exists) then
      nfiles = 1
   else
      i = 1
      do
         fn = filename(i,filebase,multi=.true.)
         inquire(file=trim(fn),exist=multiple_file_exists)
         if (.not.multiple_file_exists) exit
         i = i+1
      end do
      if (i==1) then
         call out('Error: There is only one sub-file in the base')
         call out(trim(filebase))
         stop
      end if
      nfiles = i
   end if
   
end function get_number_of_subfiles

subroutine load_parameters(forced_snapshot)

   implicit none
   character(255),intent(in)  :: forced_snapshot
   character(255)             :: line
   character(50)              :: var_name
   character(205)             :: var_value
   character(255)             :: current
   integer                    :: io
   logical                    :: parameter_written(12)
   integer*4                  :: i
   
   call check_exists(para%parameterfile)
   
   if (trim(forced_snapshot).ne.'') para%snapshot = forced_snapshot
   
   parameter_written = .false.
   current = ''
   
   open(1,file=trim(para%parameterfile),action='read',form='formatted')
   do
      read(1,'(A)',IOSTAT=io) line
      if (io.ne.0) exit
      if (.not.((len(trim(line))==0).or.(line(1:1)=='#'))) then
         read(line,*) var_name
         var_value = adjustl(line(len(trim(var_name))+1:len(line)))
         select case (trim(var_name))
            case ('default')
               current = 'default'
            case ('simulation')
               if (para%simulation=='') para%simulation = trim(var_value)
               current = trim(var_value)
            case ('snapshot')
               if (iscurrent(1).and.(trim(forced_snapshot)=='')) para%snapshot = trim(var_value)
            case ('L')
               if (iscurrent(2)) read(var_value,*) para%L
            case ('N')
               if (iscurrent(3)) read(var_value,*) para%N
            case ('path_gadget')
               if (iscurrent(4)) para%path_gadget = trim(noslash(var_value))//'/'
            case ('path_velociraptor')
               if (iscurrent(5)) para%path_velociraptor = trim(noslash(var_value))//'/'
            case ('path_surfsuite')
               if (iscurrent(6)) then
                  para%path_surfsuite = trim(noslash(var_value))//'/'
                  call system('mkdir -p '//trim(para%path_surfsuite))
               end if
            case ('path_analysis')
               if (iscurrent(7)) then
                  para%path_analysis = trim(noslash(var_value))//'/'
                  call system('mkdir -p '//trim(para%path_analysis))
               end if
            case ('ext_groups')
               if (iscurrent(8)) para%ext_groups = trim(var_value)
            case ('ext_particles')
               if (iscurrent(9)) para%ext_particles = trim(var_value)
            case ('ext_sorted')
               if (iscurrent(10)) para%ext_sorted = trim(var_value)
            case ('ext_halos')
               if (iscurrent(11)) para%ext_halos = trim(var_value)
            case ('ext_halolist')
               if (iscurrent(12)) para%ext_halolist = trim(var_value)
            case default
               call out('ERROR: '//trim(var_name)//' is an unknown parameter.')
               stop
         end select
      end if
   end do
   close(1)
   
   do i = 1,size(parameter_written)
      if (.not.parameter_written(i)) then
         call out('ERROR: not all parameters specified in parameter file.')
         stop
      end if
   end do
   
   contains
   
   logical function iscurrent(index)
      implicit none
      integer*4,intent(in) :: index
      if (trim(current)=='') then
         call out('ERROR: parameter file must start with "simulation" or "default" statement')
         stop
      end if
      if (trim(current)==trim(para%simulation)) then
         parameter_written(index) = .true.
         iscurrent = .true.
      else if (trim(current)=='default') then
         if (parameter_written(index)) then
            iscurrent = .false.
         else
            parameter_written(index) = .true.
            iscurrent = .true.
         end if
      else
         iscurrent = .false.
      end if
   end function iscurrent
   
end subroutine load_parameters

subroutine save_particles_sorted_format(index)

   implicit none
   integer,intent(in)   :: index
   character(len=255)   :: fn
   integer*8            :: i

   call tic
   
   ! write user info
   fn = filename(index,para%path_surfsuite,para%snapshot,para%ext_sorted)
   call out('SAVE FILE '//trim(fn))
   call out('Number of particles:',nparticles)

   ! write data
   open(1,file=trim(fn),action='write',form='unformatted',status='replace',access='stream')
   write(1) (p(i),i=1,nparticles)
   close(1)
   
   call toc

end subroutine save_particles_sorted_format

subroutine load_particles_sorted_format(index)

   implicit none
   integer,intent(in)   :: index
   character(len=255)   :: fn
   integer*8            :: i,file_size
   
   call tic

   ! write user info
   fn = filename(index,para%path_surfsuite,para%snapshot,para%ext_sorted)
   call out('LOAD FILE '//trim(fn))

   ! determine number of particles from file size
   inquire(file=trim(fn), size=file_size)
   nparticles = file_size/bytes_per_particle
   
   if (nparticles>0) then
   
      call out('Number of particles:',nparticles)
      if (allocated(p)) deallocate(p)
      allocate(p(nparticles))
      open(1,file=trim(fn),action='read',form='unformatted',status='old',access='stream')
      read(1) (p(i),i=1,nparticles)
      close(1)
      
   else
      
      call out('Error: Empty file '//trim(fn))
      deallocate(p)
      
   end if
   
   call toc

end subroutine load_particles_sorted_format

end module module_io