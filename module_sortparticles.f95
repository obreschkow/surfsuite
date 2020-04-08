module module_sortparticles

use shared_module_interface
use shared_module_system
use module_global
use module_io

private 
public   :: task_sortparticles
public   :: task_getgadgetproperties

integer*4               :: nfiles_gadget_unsorted
integer*4               :: nfiles_sorted_particles
integer*8               :: nparticles_tot

contains

! =================================================================================================================
! MAIN SUBROUTINES
! =================================================================================================================

subroutine task_sortparticles

   implicit none
   integer*4            :: i
   character(len=255)   :: fn
   
   call task_getgadgetproperties
   
   nfiles_sorted_particles = int(nparticles_tot/nparticles_per_sorted_file,4)
   if (nfiles_sorted_particles*nparticles_per_sorted_file<nparticles_tot) then
      nfiles_sorted_particles = nfiles_sorted_particles+1
   end if
   
   call split_particles_into_sort_files
   
   do i = 0,nfiles_sorted_particles-1
   
      fn = filename(i,para%path_surfsuite,snfile(para%snapshot),para%ext_sorted)
      call tic('PROCESS FILE '//trim(fn))
      call out('Load particles from file')
      call load_particles_sorted_format(i)
      call out('Sort particles by increasing ID')
      call sort_particles
      call out('Save particles to file')
      call save_particles_sorted_format(i)
      call toc
      deallocate(p)
      
   end do
   
end subroutine task_sortparticles

subroutine sort_particles

   implicit none
   
   integer*8,allocatable         :: sortlist(:)
   integer*8                     :: i
   integer*8                     :: id0,id1
   
   id0 = minval(p%id)-1
   id1 = maxval(p%id)
   
   if (id1-id0.ne.size(p)) call error('indexing mistake.')

   allocate(sortlist(nparticles))
   do i = 1,nparticles
      sortlist(p(i)%id-id0) = i
   end do
   
   p = p(sortlist)

end subroutine sort_particles

subroutine split_particles_into_sort_files

   ! variable declaration
   implicit none
   integer*8,parameter  :: nchunk_max = 10000000_8
   character(len=255)   :: fn
   integer*4            :: np(6)
   integer*4            :: ifile_sorted,ifile_gadget
   integer*8            :: position
   integer*8            :: i
   integer*8            :: nchunk
   integer*4,allocatable:: id4(:)
   integer*4            :: bytesid
   integer*4            :: species
   real*4               :: xmin,xmax
   integer*8            :: idmin,idmax
   integer*8            :: iparticles,iparticles_species,iparticles_tot
   
   xmin = huge(xmin)
   xmax = 0
   idmin = huge(idmin)
   idmax = 0
   
   if (nparticles_tot<=2147483648_8) then
      bytesid = 4
   else
      bytesid = 8
   end if
   
   ! allocate particle array
   if (allocated(p)) deallocate(p)
   allocate(p(nchunk_max),id4(nchunk_max))
   
   ! open all sorted files
   do ifile_sorted = 0,nfiles_sorted_particles-1
      fn = filename(ifile_sorted,para%path_surfsuite,snfile(para%snapshot),para%ext_sorted,multi=nfiles_sorted_particles>1)
      open(ifile_sorted+1000,file=trim(fn),action='write',form='unformatted',access='stream',status='replace')
   end do
   
   ! iterate over all gadget files
   iparticles_tot = 0
   do ifile_gadget = 0,nfiles_gadget_unsorted-1
   
      ! load file
      fn = filename(ifile_gadget,para%path_gadget,snfile(para%snapshot))
      
      call tic('SPLIT GADGET FILE INTO SORTABLE FILES')
      call out('Filename: ',trim(fn))
   
      ! read header
      open(1,file=trim(fn),action='read',form='unformatted',status='old',access='stream')
      read(1,pos=5) np
      nparticles = sum(np)
      call out('Number of particles: ',nparticles)
      
      iparticles = 0
      
      do species = 1,6
      
         if (np(species)>0) then
      
            iparticles_species = 0
            p%species = species
         
            do while (iparticles_species<np(species))
      
               nchunk = min(nchunk_max,np(species)-iparticles_species)
      
               ! read positions
               position = 269+iparticles*12
               read(1,pos=position) (p(i)%x(1),p(i)%x(2),p(i)%x(3),i=1,nchunk)
   
               ! read velocities
               position = 277+nparticles*12+iparticles*12
               read(1,pos=position) (p(i)%v(1),p(i)%v(2),p(i)%v(3),i=1,nchunk)
         
               ! read IDs
               position = 285+nparticles*24+iparticles*bytesid
               if (bytesid==4) then
                  read(1,pos=position) (id4(i),i=1,nchunk)
                  p%id = abs(int(id4,8)) ! abs needed, because of id 2147483648, which is out of 4-byte range in fortran
               else
                  read(1,pos=position) (p(i)%id,i=1,nchunk)
               end if
         
               ! measure ranges
               xmin = minval((/xmin,minval(p(1:nchunk)%x(1)),minval(p(1:nchunk)%x(2)),minval(p(1:nchunk)%x(3))/))
               xmax = maxval((/xmax,maxval(p(1:nchunk)%x(1)),maxval(p(1:nchunk)%x(2)),maxval(p(1:nchunk)%x(3))/))
               idmin = min(idmin,minval(p(1:nchunk)%id))
               idmax = max(idmax,maxval(p(1:nchunk)%id))
   
               ! write particles into sorted files
               do i = 1,nchunk
                  ifile_sorted = int((p(i)%id-1)/nparticles_per_sorted_file,4)
                  write(ifile_sorted+1000) p(i)
               end do
         
               ! count finished particles
               iparticles = iparticles+nchunk
               iparticles_species = iparticles_species+nchunk
               iparticles_tot = iparticles_tot+nchunk
               
               ! progress report
               call progress(real(iparticles)/real(nparticles))
               
            end do
            
         end if
         
      end do
   
      close(1)
      call toc
      
   end do
   
   deallocate(p)
   
   ! close all sorted files
   do ifile_sorted = 0,nfiles_sorted_particles-1
      close(ifile_sorted+1000)
   end do
   
   ! final checks
   if (iparticles_tot.ne.nparticles_tot) call error('wrong particle count.')
   if (xmin<0) call error('xmin should not be smaller than 0.')
   if (xmin>para%L) call error('xmin should not be larger than box length L.')
   if (idmin.ne.1) call error('minimal particle ID must be 1.')
   if (idmax.ne.nparticles_tot) call out('maximal particle ID must be equal to the number of particles.')
   
end subroutine split_particles_into_sort_files

subroutine task_getgadgetproperties

   implicit none
   character(len=255)   :: fn
   integer*8            :: file_size
   integer*4            :: i,extra
   integer*4            :: np(6),box
   integer*8            :: np_tot(6)
   character(len=255)   :: txt
   
   call hline
   
   call out('SUMMARY OF CURRENT SIMULAITON DATA')
   
   call out('Simulation: ',trim(para%simulation))
   
   call out('Snapshot: ',para%snapshot)
   
   nfiles_gadget_unsorted = get_number_of_subfiles(trim(para%path_gadget)//trim(snfile(para%snapshot)))
   call out('Number of Gadget files for this snapshot: ',nfiles_gadget_unsorted)
   
   ! compute number of particles & check file sizes
   nparticles_tot = 0
   np_tot = 0
   do i = 0,nfiles_gadget_unsorted-1
      fn = filename(i,para%path_gadget,snfile(para%snapshot))
      call checkfile(fn)
      open(1,file=trim(fn),action='read',form='unformatted',status='old')
      read(1) np
      close(1)
      nparticles_tot = nparticles_tot+sum(np)
      np_tot = np_tot+np
      
      inquire(file=trim(fn),size=file_size)
      extra = int((file_size-288_8)/sum(np)-32,4)
      if (extra<-5) then ! not 0, because 32-bytes is only true when IDs are stored as int*8, not int*4
         call error('Format of input file not recognized.')
      else if ((extra>0).and.(i==0)) then
         call out('Warning: File contains more particle data than positions, velocities and IDs.')
         call out('         These additional data are be ignored by surfsuite.')
      end if
         
   end do
   
   write(txt,'(A,I0,A,I0,A,I0,A,I0,A,I0,A,I0,A,I0,A)') &
      & 'Total number of particles: ',nparticles_tot,' (',np_tot(1),',',np_tot(2),',',np_tot(3),',',&
      & np_tot(4),',',np_tot(5),',',np_tot(6),')'
   call out(txt)
   
   ! determine number of particles per dimension
   box = 0
   do i = 1,6
      if (np_tot(i)>0) then
         if (box==0) then
            box = int(real(np_tot(i),8)**(1.0_8/3.0_8)+0.5_8,4)
         end if
         if (np_tot(i).ne.int(box,8)**3) call error('Number of particles is not a cubic number.')
      end if
   end do
   write(txt,'(A,I0,A,I0,A)') &
      & 'This equals to ',box,'^3 particles (for ',int(nparticles_tot/int(box,8)**3,4),' species).'
   call out(txt)
   
   call hline
   
end subroutine task_getgadgetproperties

end module module_sortparticles