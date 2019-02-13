module module_renderhalo

   use module_global
   use module_system
   use module_io
   use module_gethalo

   implicit none

   type type_render_options
      integer*4   :: npixels
      real*4      :: sidelength ! [simulation units]
      real*4      :: rotationvector(3) ! [rad]
      real*4      :: smoothinglength  ! [simulation units]
      real*4      :: gamma
      real*4      :: lum
   end type type_render_options

   type(type_render_options)  :: graph
   real*4,allocatable         :: rgb(:,:,:)

contains

subroutine task_renderhalo

   implicit none
   
   integer*4                  :: haloid
   character(len=255)         :: arg_option
   character(len=255)         :: arg_value
   integer*4                  :: i
   character(len=255)         :: fnbase
   logical                    :: output
   character(len=255)         :: outputfile
   integer*4                  :: mode ! 1=binary, 2=ascii
   integer*4                  :: subhalos
   type(type_halo)            :: halo
   
   ! checks
   if (narg<2) then
      call out('ERROR: Argument missing. Use')
      call out(module_gethalo_use)
      stop
   else
      call getarg(2,arg_value)
      read(arg_value,*) haloid
   end if
   
   ! default options
   output = .false.
   mode = 0
   subhalos = 0
   
   ! change default options
   if (narg>2) then
      if ((narg+1)/2.eq.(narg+1)/2.0) then
         call out('ERROR: Every option "-option" must have exactly one value.')
         stop
      end if
      do i = 3,narg,2
         call getarg(i,arg_option)
         call getarg(i+1,arg_value)
         select case (trim(arg_option))
         case ('-outputfile')
            output = .true.
            outputfile = trim(arg_value)
         case ('-mode')
            read(arg_value,*) mode
            if ((mode<0).or.(mode>2)) then
               call out('Error: mode must be 0, 1 or 2.')
               stop
            end if
         case ('-subhalos')
            read(arg_value,*) subhalos
            if ((subhalos<0).or.(subhalos>1)) then
               call out('Error: subhalos must be 0 or 1.')
               stop
            end if
         end select
      end do
   end if
   
   graph%npixels = 800
   graph%sidelength = 2
   graph%rotationvector = (/0.0,0.0,0.0/)
   graph%smoothinglength = 0.1
   graph%gamma = 0.6
   graph%lum = 1
   
   call load_halo_properties(haloid,halo)
   call load_halo_particles(haloid,subhalos==1,.true.)
   call raster_halo(mode)
   
   if (output) then
      call raster_to_bitmap(rgb,outputfile)
   else
      call raster_to_bitmap(rgb,'.tmp.bmp')
      call system('open .tmp.bmp')
   end if

end subroutine task_renderhalo

subroutine raster_halo(mode)

   implicit none
   integer*4,intent(in)          :: mode
   real*4,allocatable            :: f(:,:,:),x(:),y(:)
   real*4,allocatable            :: kernel(:,:,:)
   integer*4                     :: nsmooth,npx
   integer*4                     :: kernelsteps = 50
   integer*4                     :: species,i,j,k,ix,iy,n
   real*4                        :: q0,q,factor,x0,y0,dx,dy,dt,density
   real*8                        :: rrms,vrms
   
   ! make smoothing kernel
   nsmooth = int(graph%npixels*graph%smoothinglength/graph%sidelength*2)
   allocate(kernel(-nsmooth:nsmooth,-nsmooth:nsmooth,kernelsteps))
   do i = -nsmooth,nsmooth
      do j = -nsmooth,nsmooth
         q0 = sqrt(real(i**2+j**2))/nsmooth*2
         do k = 1,kernelsteps
            factor = 1+0.015*(k-1)**2.0
            q = q0*factor
            if (q<1) then
               kernel(i,j,k) = (1-1.5*q**2+0.75*q**3)
            else if (q<2) then
               kernel(i,j,k) = (0.25*(2-q)**3)
            else
               kernel(i,j,k) = 0
            end if
         end do
      end do
   end do
   do k = 1,kernelsteps
      kernel(:,:,k) = kernel(:,:,k)/sum(kernel(:,:,k))
   end do
   
   ! determine dt
   rrms = sqrt(sum(real(p%x**2+p%y**2,8))/nparticles)
   vrms = sqrt(sum(real(p%vx**2+p%vy**2,8))/nparticles)
   dt = real(rrms/vrms*0.3,4)
   density = nparticles/(rrms/graph%sidelength*graph%npixels)**2
   npx = max(1,nint(dt*vrms/graph%sidelength*graph%npixels))
   
   ! rotation
   allocate(x(nparticles),y(nparticles))
   x = p%x
   y = p%y
   !p%x = x*cos(1.0)+y*sin(1.0)
   !p%y = y*cos(1.0)-x*sin(1.0)
   
   ! raster channels
   allocate(f(1-2*nsmooth:graph%npixels+2*nsmooth,1-2*nsmooth:graph%npixels+2*nsmooth,6))
   f = 0
   
   if (mode == 0) then
   
      do i = 1,nparticles
         ix = nint((p(i)%x/graph%sidelength+0.5)*graph%npixels)
         if ((ix>=1-nsmooth).and.(ix<=graph%npixels+nsmooth)) then
            iy = nint((p(i)%y/graph%sidelength+0.5)*graph%npixels)
            if ((iy>=1).and.(iy<=graph%npixels)) then
               f(ix,iy,p(i)%typ) = f(ix,iy,p(i)%typ)+1
            end if
         end if
      end do
   
   else if (mode == 1) then
   
      do i = 1,nparticles
         ix = nint((p(i)%x/graph%sidelength+0.5)*graph%npixels)
         if ((ix>=1-nsmooth).and.(ix<=graph%npixels+nsmooth)) then
            iy = nint((p(i)%y/graph%sidelength+0.5)*graph%npixels)
            if ((iy>=1).and.(iy<=graph%npixels)) then
               k = min(nint(f(ix,iy,p(i)%typ)/density**2*12000+1),kernelsteps)
               f(ix-nsmooth:ix+nsmooth,iy-nsmooth:iy+nsmooth,p(i)%typ) = &
               &  f(ix-nsmooth:ix+nsmooth,iy-nsmooth:iy+nsmooth,p(i)%typ)+kernel(:,:,k)
            end if
         end if
      end do
      
   else if (mode == 2) then
   
      do i = 1,nparticles
         x0 = (p(i)%x/graph%sidelength+0.5)*graph%npixels
         if ((x0>=1-nsmooth).and.(x0<=graph%npixels+nsmooth)) then
            y0 = (p(i)%y/graph%sidelength+0.5)*graph%npixels
            if ((y0>=1).and.(y0<=graph%npixels)) then
               dx = p(i)%vx*dt/graph%sidelength*graph%npixels
               if ((x0+dx>=1-nsmooth).and.(x0+dx<=graph%npixels+nsmooth)) then
                  dy = p(i)%vy*dt/graph%sidelength*graph%npixels
                  if ((y0+dy>=1).and.(y0+dy<=graph%npixels)) then
                     n = nint(sqrt(dx**2+dy**2))+1
                     do k = -n,n
                        ix = nint(x0+real(k)/n/2*dx)
                        iy = nint(y0+real(k)/n/2*dy)
                        f(ix,iy,p(i)%typ) = f(ix,iy,p(i)%typ)+1
                     end do
                  end if
               end if
            end if
         end if
      end do
      f = f/npx
   
   end if
   
   ! contrast
   f = f/density*2
   f = f**graph%gamma*graph%lum
   f = 1.0-exp(-f)
   if ((mode == 0).or.(mode == 2)) where(f>0) f=0.1+0.9*f
   
   ! convert to rgb
   allocate(rgb(graph%npixels,graph%npixels,3))
   rgb = 0
   rgb(:,:,1) = f(1:graph%npixels,1:graph%npixels,1)+0.2*f(1:graph%npixels,1:graph%npixels,2)
   rgb(:,:,2) = 0.2*f(1:graph%npixels,1:graph%npixels,2)
   rgb(:,:,3) = f(1:graph%npixels,1:graph%npixels,2)
   where (rgb>1) rgb = 1
   
end subroutine raster_halo

subroutine raster_to_bitmap(rgb,filename)

   ! converts rgb(1:height,1:width,1:3) [0...1] to a 24-bit bitmap file

   implicit none
   character(*),intent(in) :: filename
   real*4,intent(in)       :: rgb(:,:,:)
   integer*4               :: width,height
   integer*4               :: extrabytes
   integer*4               :: paddedsize
   integer*4               :: i,j,k
   
   height = size(rgb,1)
   width = size(rgb,2)
   extrabytes = 4-modulo(width*3,4) ! number of bytes of padding to add to each horizontal line
   if (extrabytes == 4) extrabytes = 0
   paddedsize = ((width*3)+extrabytes)*height

   open(1,file=trim(filename),action='write',form='unformatted',status='replace',access='stream')
   
   ! write header
   write(1) 'BM',paddedsize+54,0,54 ! BMP header
   write(1) 40,width,height,achar(1),achar(0),achar(24),achar(0),0,paddedsize,2835,2835,0,0 ! DIB header
   
   ! write array
   do i = 1,height
      do j = 1,width
         do k = 3,1,-1
            write(1) achar(nint(rgb(i,j,k)*255))
         end do
      end do
      do j = 1,extrabytes
         write(1) achar(0)
      end do
   end do
   
   close(1)

end subroutine raster_to_bitmap

end module module_renderhalo