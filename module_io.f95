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
   if ((.not.res).and.(.not.present(do_not_stop))) call error('File does not exist: '//trim(filename))
end function exists

subroutine check_exists(filename)
   implicit none
   character(*),intent(in) :: filename
   if (.not.exists(filename)) stop
end subroutine check_exists

function snfile(sn) result(fn)

   implicit none
   integer,intent(in)   :: sn ! snapshot number
   character(len=255)   :: fn
   
   write(fn,'('//trim(para%snapshot_fmt)//')') trim(para%snapshot_prefix),sn

end function snfile

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
         if (index>0) call error('Error in function filename(): single file with index>0')
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
      call out('ERROR: Could not find any files for the base')
      call out(trim(filebase))
      stop
   else if (single_file_exists.and.multiple_file_exists) then
      call out('ERROR: Ambiguous, since single and multiple files exist for the base')
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
         call out('ERROR: There is only one sub-file in the base')
         call out(trim(filebase))
         stop
      end if
      nfiles = i
   end if
   
end function get_number_of_subfiles

subroutine load_parameters(forced_snapshot)

   implicit none
   integer*4,intent(in)       :: forced_snapshot
   character(255)             :: line
   character(50)              :: var_name
   character(205)             :: var_value
   character(255)             :: current
   integer                    :: io
   logical                    :: parameter_written(15)
   integer*4                  :: i
   logical                    :: simulation_exists
   integer*4                  :: status
   
   call check_exists(para%parameterfile)
   
   if (forced_snapshot>=0) para%snapshot = forced_snapshot
   
   parameter_written = .false.
   current = ''
   simulation_exists = .false.
   
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
               if (para%simulation=='') then
                  para%simulation = trim(var_value)
                  simulation_exists = .true.
               else
                  if (para%simulation == trim(var_value)) simulation_exists = .true.
               end if
               current = trim(var_value)
            case ('snapshot')
               if (iscurrent(1).and.(forced_snapshot<0)) read(var_value,*) para%snapshot
            case ('L')
               if (iscurrent(2)) read(var_value,*) para%L
            case ('N')
               if (iscurrent(3)) read(var_value,*) para%N
            case ('path_gadget')
               if (iscurrent(4)) para%path_gadget = trim(noslash(var_value))//'/'
            case ('path_velociraptor')
               if (iscurrent(5)) para%path_velociraptor = trim(noslash(var_value))//'/'
            case ('path_surfsuite')
               if (iscurrent(6)) para%path_surfsuite = trim(noslash(var_value))//'/'
            case ('path_analysis')
               if (iscurrent(7)) para%path_analysis = trim(noslash(var_value))//'/'
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
            case ('snapshot_fmt')
               if (iscurrent(13)) para%snapshot_fmt = trim(var_value)
            case ('snapshot_prefix')
               if (iscurrent(14)) para%snapshot_prefix = trim(var_value)
            case ('file_scalefactors')
               if (iscurrent(15)) para%file_scalefactors = trim(var_value)
            case default
               call error(trim(var_name)//' is an unknown parameter.')
         end select
      end if
   end do
   close(1)
   
   ! check parameter existence
   if (.not.simulation_exists) call error('specified simulation does not exist in parameter file.')
   do i = 1,size(parameter_written)
      if (.not.parameter_written(i)) call error('not all parameters specified in parameter file.')
   end do
   
   ! check paths and files
   if (.not.exists(para%path_gadget,.false.)) then
      call out('ERROR: Could not find directory specified by path_gadget:')
      call out(trim(para%path_gadget))
      call out('Consider specifying a different simulation using "-simulation" or')
      call out('changing the paths in the parameter file.')
      stop
   end if
   if (.not.exists(para%file_scalefactors,.false.)) then
      call out('ERROR: Could not find file specified by file_scalefactors:')
      call out(trim(para%file_scalefactors))
      stop
   else
      call load_scalefactors
   end if
   
   ! check physical parameters
   if (para%L<=0) call error('L must be a positive real.')
   if (para%N<=0) call error('L must be a positive real.')
   
   ! make paths, if not already existing
   status = system('mkdir -p '//trim(para%path_surfsuite))
   if (status.ne.0) then
      call out('ERROR: You do not have read/write permissions to the path specified by path_surfsuite:')
      call out(trim(para%path_surfsuite))
      stop
   end if
   status = system('mkdir -p '//trim(para%path_analysis))
   if (status.ne.0) then
      call out('ERROR: You do not have read/write permissions to the path specified by path_analysis:')
      call out(trim(para%path_analysis))
      stop
   end if
   
   contains
   
   logical function iscurrent(index)
      implicit none
      integer*4,intent(in) :: index
      if (trim(current)=='') call error('parameter file must start with "simulation" or "default" statement')
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

subroutine load_scalefactors

   implicit none
   
   integer*4   :: snapshot,snapshot_min,snapshot_max,snapshot_expected
   integer*4   :: status
   real*4      :: a,aold
   
   ! determine minimum and maximum snapshot
   snapshot_min = huge(snapshot_min)
   snapshot_max = -huge(snapshot_max)
   open(1,file=trim(para%file_scalefactors),action='read',form='formatted')
   do
      read(1,*,IOSTAT=status) snapshot,a
      if (status>0) then
         call error('unknown format of scale factor file')
      else if (status<0) then
         exit ! end of file
      else
         snapshot_min = min(snapshot_min,snapshot)
         snapshot_max = max(snapshot_max,snapshot)
      end if
   end do
   close(1)

   ! read scale factors
   if (allocated(scalefactor)) deallocate(scalefactor)
   allocate(scalefactor(snapshot_min:snapshot_max))
   snapshot_expected = snapshot_min
   aold = 0.0
   open(1,file=trim(para%file_scalefactors),action='read',form='formatted')
   do
      read(1,*,IOSTAT=status) snapshot,a
      if (status>0) then
         call error('unknown format of scale factor file')
      else if (status<0) then
         exit ! end of file
      else
         if (snapshot.ne.snapshot_expected) call error('snapshots in scale factor file must be in increasing order')
         if (a<aold) call error('scale factors must be increasing with increasing snapshot index')
         scalefactor(snapshot) = a
         snapshot_expected = snapshot+1
      end if
   end do
   close(1)

end subroutine load_scalefactors

subroutine save_particles_sorted_format(index)

   implicit none
   integer,intent(in)   :: index
   character(len=255)   :: fn
   integer*8            :: i

   call tic
   
   ! write user info
   fn = filename(index,para%path_surfsuite,snfile(para%snapshot),para%ext_sorted)
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
   fn = filename(index,para%path_surfsuite,snfile(para%snapshot),para%ext_sorted)
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
      
      call error('Empty file '//trim(fn))
      deallocate(p)
      
   end if
   
   call toc

end subroutine load_particles_sorted_format

end module module_io