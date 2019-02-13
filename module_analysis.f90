module module_analysis

use module_global
use module_system
use module_io
use module_gethalo

implicit none

logical,parameter    :: compute_uncertainties = .false.
logical,parameter    :: include_subhaloes = .true. ! if true, subhalos are treated as part of their parent halos; if false, they are treated independently
integer*4,parameter  :: sdf_nr = 32
integer*4,parameter  :: sdf_ncostheta = 32
real*4,parameter     :: sdf_rmax = 1.0 ! [rvir]
integer*4,parameter  :: ampdf_imax = 32
real*4,parameter     :: ampdf_jmax = 5.0 ! [jtotal]
real*4,parameter     :: h = 0.6751
real*4,parameter     :: OmegaM = 0.3121
real*4,parameter     :: OmegaL = 1-OmegaM
real*4,parameter     :: redshift = 0 ! needed for calculation of Rvir
real*4               :: mpart ! [Msun/h] particle mass
integer*4            :: nh ! number of halos

type type_md ! Mass distribution

   ! spatial distribution function
   real*4      :: dV(1:sdf_nr,1:sdf_ncostheta)     ! bin volume
   real*4      :: sdf(1:sdf_nr,1:sdf_ncostheta)    ! density
   real*4      :: centralfaction

end type type_md

type type_amd ! AM distribution

   ! spatial distribution function
   real*4      :: sdf(1:sdf_nr,1:sdf_ncostheta)
   integer*4   :: sdf_n(1:sdf_nr,1:sdf_ncostheta)
   
   ! probability distribution function
   real*4      :: pdf_axis(-ampdf_imax:ampdf_imax)
   real*4      :: pdf_perp(-ampdf_imax:ampdf_imax)
   real*4      :: pdf_axis_sd(-ampdf_imax:ampdf_imax)
   real*4      :: pdf_perp_sd(-ampdf_imax:ampdf_imax)

end type type_amd

type type_derived_halo_properties
   
   real*4         :: mvir
   real*4         :: rvir
   real*4         :: j(3)
   real*4         :: jnorm
   type(type_md)  :: md
   type(type_amd) :: amd

end type type_derived_halo_properties

type(type_derived_halo_properties)  :: derived
type(type_derived_halo_properties)  :: average

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
      call make_distributions
   case (2)
      call make_bullet_indices
   case default
      write(*,*) 'ERROR: '//trim(arg_value)//' is an unknown analysis mode.'
      stop
   end select
   call toc()
   
end subroutine task_analysis

subroutine make_bullet_indices
   
   implicit none
   
   logical,parameter          :: save_particles = .false.
   real*4,parameter           :: Mmin = 1e11 ! [Msun/h]
   real*4,parameter           :: Mmin_save_particles = 1e13 ! [Msun/h]
   integer*4                  :: i,species,ntot,ntyp(2),nmin
   integer*8                  :: j
   real*4                     :: A(3,3),quadrupole_amplitude(2)
   real*4                     :: bullet_index,mvir,values(3),vectors(3,3)
   character(len=255)         :: fn
   type(type_halo) :: halo
   character(len=8)           :: prefix
   real*4,parameter           :: fb = 0.1573 ! baryon fraction relative to all matter
   real*4                     :: mtyp(2),mpartgas,mpartCDM
   
   mpartgas = fb*mpart     ! [Msun/h]
   mpartCDM = (1-fb)*mpart ! [Msun/h]
   
   fn = trim(para%path_analysis)//trim(para%snapshot)//'_bulletindex'
   open(10,file=trim(fn),action='write',status='replace',form='unformatted',access='stream')
   
   if (save_particles) then
      write(prefix,'(A,F4.1)') 'logM',log10(Mmin_save_particles) 
      fn = trim(para%path_analysis)//trim(para%snapshot)//'_'//prefix//'_haloparticles_only_bullets'
      open(11,file=trim(fn),action='write',form='unformatted',status='replace',access='stream')
   
      fn = trim(para%path_analysis)//trim(para%snapshot)//'_'//prefix//'_haloparticles_without_bullets'
      open(12,file=trim(fn),action='write',form='unformatted',status='replace',access='stream')
   
      fn = trim(para%path_analysis)//trim(para%snapshot)//'_'//prefix//'_haloparticles_species_1'
      open(13,file=trim(fn),action='write',form='unformatted',status='replace',access='stream')
   
      fn = trim(para%path_analysis)//trim(para%snapshot)//'_'//prefix//'_haloparticles_species_2'
      open(14,file=trim(fn),action='write',form='unformatted',status='replace',access='stream')
   end if
   
   nmin = floor(Mmin/mpartCDM)
   
   do i = 1,nh
      
      call load_halo_properties(i,halo)
      
      if (halo%parentid==-1) then ! selecting parent haloes
      
         ntot = halo%npart+halo%npartsub
         
         if (ntot>=nmin) then
   
            call load_halo_particles(i,.true.,.true.)
            ntyp(1) = count(p%typ==1)
            ntyp(2) = count(p%typ==2)
            mtyp(1) = mpartgas*ntyp(1) ! [Msun/h]
            mtyp(2) = mpartCDM*ntyp(2) ! [Msun/h]
            
            if (sum(mtyp)>=Mmin) then
            
               if ((ntyp(1)>=20).and.(ntyp(2)>=20)) then
            
                  ! compute mass of selected halos
                  mvir = sum(mtyp)        ! [Msun/h]
      
                  ! compute bullet index of selected halos
                  do species = 1,2
                     call make_quadrupole_tensor(species,A)
                     call eigen(A,values,vectors)
                     quadrupole_amplitude(species) = maxval(values)/ntyp(species) ! maximal quadrupole eigenvalue per unit mass
                  end do
                  bullet_index = (quadrupole_amplitude(2)-quadrupole_amplitude(1))/sum(quadrupole_amplitude) ! bound between -1 (dumbbell of gas) and +1 (dumbbell of CDM)
            
                  ! save global properties
                  write(10) i,mvir,bullet_index,values,vectors
            
                  ! save particles of selected halos
                  if (save_particles) then
                     if (mvir>=Mmin_save_particles) then
                        call load_halo_particles(i,.true.,.false.)
                        if (bullet_index>0.2) then
                           write(11) (p(j)%x,p(j)%y,p(j)%z,j=1,nparticles)
                        end if
                        if (bullet_index<0.2) then
                           write(12) (p(j)%x,p(j)%y,p(j)%z,j=1,nparticles)
                        end if
                        do j = 1,nparticles
                           write(12+p(j)%typ) p(j)%x,p(j)%y,p(j)%z
                        end do
                     end if
                  end if
                  
               end if
               
            end if
      
         end if
         
      end if
      
   end do
   
   close(10)
   if (save_particles) then
      close(11)
      close(12)
      close(13)
      close(14)
   end if
   
end subroutine make_bullet_indices

subroutine make_quadrupole_tensor(species,quadrupole)

   ! returns the quadrupole tensor
   
   implicit none
   integer*4,intent(in) :: species
   real*4,intent(out)   :: quadrupole(3,3)
   integer*8            :: i
   
   quadrupole = 0
   do i = 1,nparticles
      if (p(i)%typ==species) then
         quadrupole(1,1) = quadrupole(1,1)+2*p(i)%x**2-p(i)%y**2-p(i)%z**2
         quadrupole(2,2) = quadrupole(2,2)+2*p(i)%y**2-p(i)%z**2-p(i)%x**2
         quadrupole(3,3) = quadrupole(3,3)+2*p(i)%z**2-p(i)%x**2-p(i)%y**2
         quadrupole(1,2) = quadrupole(1,2)+3*p(i)%x*p(i)%y
         quadrupole(2,3) = quadrupole(2,3)+3*p(i)%y*p(i)%z
         quadrupole(3,1) = quadrupole(3,1)+3*p(i)%z*p(i)%x
      end if
   end do
   quadrupole(2,1) = quadrupole(1,2)
   quadrupole(3,2) = quadrupole(2,3)
   quadrupole(1,3) = quadrupole(3,1)
   
end subroutine make_quadrupole_tensor

subroutine make_distributions

   implicit none
   integer*4                  :: i,nsel,modemax,mode
   integer*8                  :: npart_tot
   type(type_halo) :: halo
   real*8                     :: jmax
   
   npart_tot = 0
   nsel = 0
   average = empty()
   
   if (compute_uncertainties) then
      modemax = 2
   else
      modemax = 1
   end if
   
   call tic()
   
   do mode = 1,modemax
   
      do i = 1,nh
         
         call load_halo_properties(i,halo)
      
         derived = empty()
         derived%mvir = (halo%npart+halo%npartsub)*mpart/h ! [Msun]
         derived%rvir = (4.3022682e-6/h**2/(OmegaM*(1+redshift)**3+OmegaL)**2*derived%mvir)**(1.0/3.0) ! [pkpc] ? check if this is really physical or comoving
      
         if (selection()) then
            call load_halo_particles(i,include_subhaloes,.true.)
            call derive_properties
            jmax = 1.96e3*0.2*(derived%mvir/1e10)**(2.0/3.0) ! [pkpc km/s] maximally possible AM assuming a maximal spin parameter of 0.2
            
            if (derived%jnorm<jmax) then
               if (mode==1) then
                  average%mvir = average%mvir+derived%mvir
                  average%rvir = average%rvir+derived%rvir
                  average%jnorm = average%jnorm+derived%jnorm
                  average%amd%pdf_axis = average%amd%pdf_axis+derived%amd%pdf_axis
                  average%amd%pdf_perp = average%amd%pdf_perp+derived%amd%pdf_perp
                  average%md%sdf = average%md%sdf+derived%md%sdf
                  average%amd%sdf = average%amd%sdf+derived%amd%sdf*derived%amd%sdf_n
                  average%amd%sdf_n = average%amd%sdf_n+derived%amd%sdf_n
                  nsel = nsel+1
                  npart_tot = npart_tot+nparticles
               else
                  average%amd%pdf_axis_sd = average%amd%pdf_axis_sd+(derived%amd%pdf_axis-average%amd%pdf_axis)**2
                  average%amd%pdf_perp_sd = average%amd%pdf_perp_sd+(derived%amd%pdf_perp-average%amd%pdf_perp)**2
               end if
            end if
         end if
      
      end do
      
      if (nsel==0) then
         call out('Error: no halos passed the selection function.')
         stop
      end if
   
      if (mode==1) then
         average%mvir = average%mvir/nsel
         average%rvir = average%rvir/nsel
         average%jnorm = average%jnorm/nsel
         average%md%sdf = average%md%sdf/nsel
         average%amd%pdf_axis = average%amd%pdf_axis/nsel
         average%amd%pdf_perp = average%amd%pdf_perp/nsel
         average%amd%sdf = average%amd%sdf/max(1,average%amd%sdf_n)
      else
         average%amd%pdf_axis_sd = sqrt(average%amd%pdf_axis_sd/nsel)
         average%amd%pdf_perp_sd = sqrt(average%amd%pdf_perp_sd/nsel)
      end if
      
   end do
   
   call save_msdf(average)
   call save_ampdf(average)
   call save_amsdf(average)
   
   write(*,*) npart_tot
   write(*,*) nsel,average%mvir,average%jnorm
   
   contains
   
   logical function selection()
   
      implicit none
      
      if (include_subhaloes) then
         selection = halo%npart+halo%npartsub>1
         !selection = selection.and.(halo%parentid==-1)
      else
         selection = halo%npart>1
      end if
      selection = selection.and.(derived%mvir>=10**11.75)
      selection = selection.and.(derived%mvir<10**12.25)
   
   end function selection
   
end subroutine make_distributions

subroutine save_msdf(tmp)

   implicit none
   type(type_derived_halo_properties),intent(in)   :: tmp
   integer*4                                       :: bin1
   character(len=255)                              :: fn
   
   fn = trim(para%path_analysis)//trim(para%snapshot)//'_msdf.txt'
   write(*,*) trim(fn)
   open(1,file=trim(fn),action='write',status='replace',form='formatted')
   
   write(1,'(A)') '----------------------------------------------------------------------------------'
   write(1,'(A)') 'Mass Spatial Distribution Function (MSDF)'
   write(1,'(A,I0)') 'Nbins_radial   = ',sdf_nr
   write(1,'(A,I0)') 'Nbins_costheta = ',sdf_ncostheta
   write(1,'(A)') '----------------------------------------------------------------------------------'
   
   do bin1 = 1,sdf_nr
      write(1,*) tmp%md%sdf(bin1,:)
   end do
   
   close(1)

end subroutine save_msdf

subroutine save_amsdf(tmp)

   implicit none
   type(type_derived_halo_properties),intent(in)   :: tmp
   integer*4                                       :: bin1
   character(len=255)                              :: fn
   
   fn = trim(para%path_analysis)//trim(para%snapshot)//'_amsdf.txt'
   open(1,file=trim(fn),action='write',status='replace',form='formatted')
   
   write(1,'(A)') '----------------------------------------------------------------------------------'
   write(1,'(A)') 'Angular Momentum Spatial Distribution Function (AMSDF)'
   write(1,'(A,I0)') 'Nbins_radial   = ',sdf_nr
   write(1,'(A,I0)') 'Nbins_costheta = ',sdf_ncostheta
   write(1,'(A)') '----------------------------------------------------------------------------------'
   
   do bin1 = 1,sdf_nr
      write(1,*) tmp%amd%sdf(bin1,:)
   end do
   
   close(1)

end subroutine save_amsdf

subroutine save_ampdf(tmp)

   implicit none
   type(type_derived_halo_properties),intent(in)   :: tmp
   integer*4                                       :: bin
   character(len=255)                              :: fn
   
   fn = trim(para%path_analysis)//trim(para%snapshot)//'_ampdf.txt'
   open(1,file=trim(fn),action='write',status='replace',form='formatted')
   
   write(1,'(A)') '----------------------------------------------------------------------------------'
   write(1,'(A)') 'Angular Momentum Spatial Distribution Function (AMPDF)'
   write(1,'(A,I0)') 'Nbins_j   = ',ampdf_imax*2+1
   write(1,'(A)') '----------------------------------------------------------------------------------'
   
   do bin = -ampdf_imax,ampdf_imax
      write(1,'(E12.5,4E13.5)') bin/real(ampdf_imax)*ampdf_jmax,&
      &tmp%amd%pdf_axis(bin),tmp%amd%pdf_axis_sd(bin),&
      &tmp%amd%pdf_perp(bin),tmp%amd%pdf_perp_sd(bin)
   end do
   
   close(1)

end subroutine save_ampdf

subroutine derive_properties

   implicit none
   
   integer*8            :: i
   real*4,allocatable   :: pos_r(:),pos_axis(:),pos_costheta(:)
   real*4,allocatable   :: j_x(:),j_y(:),j_z(:),j_axis(:),j_perp(:)
   real*4               :: axis(3),perp(3),dj
   integer*4            :: bin,bin1,bin2
   real*4,parameter     :: pi = 3.14159265359
   real*4               :: dr,rmin,rmax,dcostheta,costhetamin,costhetamax
   real*4               :: a ! scale factor of the universe (= 1/(1+z))
   
   if (nparticles<2) then
      write(*,*) 'At least 2 particles are needed for AM calculations.'
      stop
   end if
   
   ! convert from comoving to physical units
   a = 1.0/(1.0+redshift)
   p%x = p%x/h*1e3*a ! [pkpc]
   p%y = p%y/h*1e3*a ! [pkpc]
   p%z = p%z/h*1e3*a ! [pkpc]
   
   ! center of mass specific angular momentum
   allocate(j_x(nparticles),j_y(nparticles),j_z(nparticles))
   j_x = p%y*p%vz-p%z*p%vy ! [pkpc km/s]
   j_y = p%z*p%vx-p%x*p%vz ! [pkpc km/s]
   j_z = p%x*p%vy-p%y*p%vx ! [pkpc km/s]
   derived%j(1) = sum(j_x)/real(nparticles,4) ! [pkpc km/s]
   derived%j(2) = sum(j_y)/real(nparticles,4) ! [pkpc km/s]
   derived%j(3) = sum(j_z)/real(nparticles,4) ! [pkpc km/s]
   derived%jnorm = sqrt(sum(derived%j**2)) ! [pkpc km/s]
   
   ! rotation along AM axis
   axis = derived%j/derived%jnorm
   perp = (/0.0,-axis(3),axis(2)/)
   perp = perp/sqrt(sum(perp**2))
   allocate(pos_r(nparticles),pos_costheta(nparticles),pos_axis(nparticles)) ! needed because auto-allocation with sqrt() can cause compatibility issues
   pos_r = sqrt(p%x**2+p%y**2+p%z**2)
   pos_axis = p%x*axis(1)+p%y*axis(2)+p%z*axis(3)
   pos_costheta = abs(pos_axis)/pos_r
   deallocate(pos_axis)
   j_axis = axis(1)*j_x+axis(2)*j_y+axis(3)*j_z ! [pkpc km/s]
   j_perp = perp(1)*j_x+perp(2)*j_y+perp(3)*j_z ! [pkpc km/s]
   
   ! Bin volume [units of rvir^3], also accounts for bins at theta>pi/2 => factor 4*pi instead of 2*pi
   dr = sdf_rmax/sdf_nr
   dcostheta = 1.0/sdf_ncostheta
   do bin1 = 1,sdf_nr
      rmin = (bin1-1)*dr
      rmax = bin1*dr
      do bin2 = 1,sdf_ncostheta
         costhetamin = (bin2-1)*dcostheta
         costhetamax = bin2*dcostheta
         derived%md%dV(bin1,bin2) = 4*pi/3*(rmax**3-rmin**3)*(costhetamax-costhetamin)
      end do
   end do
   
   ! Mass and AM spatial distribution function (MSDF and AMSDF)
   derived%md%centralfaction = count(pos_r<0.1*derived%rvir)/real(nparticles)
   dr = sdf_rmax/sdf_nr! [units of rvir]
   do i = 1,nparticles
      bin1 = max(1,ceiling(pos_r(i)/derived%rvir/sdf_rmax*sdf_nr))
      bin2 = max(1,min(sdf_ncostheta,ceiling(pos_costheta(i)*sdf_ncostheta)))
      if (bin1<=sdf_nr) then
         derived%md%sdf(bin1,bin2) = derived%md%sdf(bin1,bin2)+1/derived%md%dV(bin1,bin2)
         derived%amd%sdf_n(bin1,bin2) = derived%amd%sdf_n(bin1,bin2)+1
         derived%amd%sdf(bin1,bin2) = derived%amd%sdf(bin1,bin2)+j_axis(i)/derived%jnorm
      end if
   end do
   derived%amd%sdf = derived%amd%sdf/max(derived%amd%sdf_n,1)
   
   ! AM probability distribution (AMPDF)
   dj = ampdf_jmax/ampdf_imax
   do i = 1,nparticles
      bin = nint(j_axis(i)/derived%jnorm/dj)
      if (abs(bin)<=ampdf_imax) then
         derived%amd%pdf_axis(bin) = derived%amd%pdf_axis(bin)+1
      end if
      bin = nint(j_perp(i)/derived%jnorm/dj)
      if (abs(bin)<=ampdf_imax) then
         derived%amd%pdf_perp(bin) = derived%amd%pdf_perp(bin)+1
      end if
   end do
   derived%amd%pdf_axis = derived%amd%pdf_axis/real(nparticles,4)/dj
   derived%amd%pdf_perp = derived%amd%pdf_perp/real(nparticles,4)/dj
   
end subroutine derive_properties

type(type_derived_halo_properties) function empty()

   empty%mvir  = 0
   empty%rvir  = 0
   empty%j     = 0
   empty%jnorm = 0
   empty%md%dV = 0
   empty%md%sdf = 0
   empty%md%centralfaction = 0
   empty%amd%sdf = 0
   empty%amd%sdf_n = 0
   empty%amd%pdf_axis = 0
   empty%amd%pdf_perp = 0
   empty%amd%pdf_axis_sd = 0
   empty%amd%pdf_perp_sd = 0
   
end function empty
   
end module module_analysis