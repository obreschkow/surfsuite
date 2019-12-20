module module_analysis

use module_global
use module_system
use module_io
use module_gethalo
use module_processhalo

implicit none

logical,parameter    :: include_subhaloes = .false. ! if true, subhalos are treated as part of their parent halos; if false, they are treated independently
real*4,parameter     :: h = 0.6751
real*4,parameter     :: OmegaM = 0.3121
real*4,parameter     :: OmegaL = 1-OmegaM
real*4               :: mpart ! [Msun/h] particle mass
integer*4            :: nh ! number of halos

contains

subroutine task_analysis

   implicit none
   integer*4            :: mode
   character(len=255)   :: arg_value
   
   if (narg<2) then
      call out('ERROR: Argument missing.')
      stop
   else
      call getarg(2,arg_value)
      read(arg_value,*) mode
   end if
   
   nh = nhalos() ! number of halos
   mpart = para%L**3/real(para%N,4)**3*OmegaM*2.774495e11 ! [Msun/h] particle mass (in the case of hydro runs 1 CDM+ 1 baryon particle)
   
   call hline
   call tic()
   select case (mode)
   case (1)
      call make_bullet_indices
   case default
      write(*,*) 'ERROR: '//trim(arg_value)//' is an unknown analysis mode.'
      stop
   end select
   call toc()
   
end subroutine task_analysis

subroutine make_bullet_indices
   
   implicit none
   
   real*4,parameter           :: Mmin = 1e11 ! [Msun/h]
   integer*4                  :: i,species,ntot,ntyp(2),nmin
   real*4                     :: q(2),mom(2)
   real*4                     :: size_index,bullet_index,mvir
   character(len=255)         :: fn
   type(type_halo)            :: halo
   real*4,parameter           :: fb = 0.1573 ! baryon fraction relative to all matter
   real*4                     :: mtyp(2),mpartgas,mpartCDM
   
   mpartgas = fb*mpart     ! [Msun/h]
   mpartCDM = (1-fb)*mpart ! [Msun/h]
   
   fn = trim(para%path_analysis)//trim(para%snapshot)//'_bulletindex'
   open(10,file=trim(fn),action='write',status='replace',form='unformatted',access='stream')
   
   nmin = floor(Mmin/mpartCDM)
   
   do i = 1,nh
   
      if (mod(i-1,1000)==0) write(*,'(F5.1A)') i/real(nh)*100,'%'
      
      call load_halo_properties(i,halo)
      
      if (halo%parentid==-1) then ! selecting parent haloes
      
         ntot = halo%npart+halo%npartsub
         
         if (ntot>=nmin) then
   
            call load_halo(i,include_subhaloes,halo)
            call center_particles
            
            ntyp(1) = count(p%species==1)
            ntyp(2) = count(p%species==2)
            mtyp(1) = mpartgas*ntyp(1) ! [Msun/h]
            mtyp(2) = mpartCDM*ntyp(2) ! [Msun/h]
            
            if (sum(mtyp)>=Mmin) then
            
               if ((ntyp(1)>=20).and.(ntyp(2)>=20)) then
            
                  ! compute mass of selected halos
                  mvir = sum(mtyp)        ! [Msun/h]
      
                  ! compute bullet index of selected halos
                  do species = 1,2
                     call make_moments(species,q(species),mom(species))
                  end do
                  size_index = (mom(2)-mom(1))/(mom(1)+mom(2))
                  bullet_index = (q(2)-q(1))/(mom(1)+mom(2))/2 ! bound between -1 (dumbbell of gas) and +1 (dumbbell of CDM)
                  !write(*,'(I0,F11.7,F11.7)') i,size_index,bullet_index
                  
                  ! save global properties
                  write(10) real(i,4),mvir,size_index,bullet_index
                  
               end if
               
            end if
      
         end if
         
      end if
      
   end do
   
   close(10)
   
end subroutine make_bullet_indices

subroutine make_moments(species,q,m)

   implicit none
   integer*4,intent(in) :: species
   real*4,intent(out)   :: q ! largest eigen value of specific quadrupole tensor (per particle)
   real*4,intent(out)   :: m ! mean square radial distance 
   real*8               :: quadrupole(3,3),sqdist
   real*4               :: values(3),vectors(3,3)
   integer*8            :: i,count
   
   count = 0
   quadrupole = 0
   sqdist = 0
   do i = 1,nparticles
      if (p(i)%species==species) then
         count = count+1
         quadrupole(1,1) = quadrupole(1,1)+2*p(i)%x(1)**2-p(i)%x(2)**2-p(i)%x(3)**2
         quadrupole(2,2) = quadrupole(2,2)+2*p(i)%x(2)**2-p(i)%x(3)**2-p(i)%x(1)**2
         quadrupole(3,3) = quadrupole(3,3)+2*p(i)%x(3)**2-p(i)%x(1)**2-p(i)%x(2)**2
         quadrupole(1,2) = quadrupole(1,2)+3*p(i)%x(1)*p(i)%x(2)
         quadrupole(2,3) = quadrupole(2,3)+3*p(i)%x(2)*p(i)%x(3)
         quadrupole(3,1) = quadrupole(3,1)+3*p(i)%x(3)*p(i)%x(1)
         sqdist = sqdist+p(i)%x(1)**2+p(i)%x(2)**2+p(i)%x(3)**2
      end if
   end do
   quadrupole(2,1) = quadrupole(1,2)
   quadrupole(3,2) = quadrupole(2,3)
   quadrupole(1,3) = quadrupole(3,1)
   
   ! diagonalize quadrupole tensor
   call eigen(real(quadrupole,4),values,vectors)
   
   ! make outputs
   q = values(3)/real(count,4) ! maximum eigenvalue
   m = real(sqdist/real(count,4),4)
   
end subroutine make_moments
   
end module module_analysis