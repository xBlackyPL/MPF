PROGRAM imp

!   IMPLICIT NONE
   
   REAL, ALLOCATABLE, DIMENSION(:) :: x,vmin,vmax,xtemp
   REAL :: r,eps,ftemp
   INTEGER :: j,n,error,nb,maxiter
   
   ! example3 -> access to internals of C function through structure
   INTEGER :: vcl
   COMMON/v/ vcl
   
   ! Set dimension of the problem and allocate arrays:
   n=3
   ALLOCATE( x(n),     stat=error )
   ALLOCATE( vmin(n),  stat=error )
   ALLOCATE( vmax(n),  stat=error )
   ALLOCATE( xtemp(n), stat=error )
   
   ! Set number of particles:
   nb = 64
   
   ! Initialize vmin and vmax arrays:
   DO j = 1, n, 1
      vmin(j) = 0.5
      vmax(j) = 1.5
   END DO
   
   ! Set maximum precision (used for stopping criterion)
   ! and maximum number of iterations:
   eps = 0.00001
   maxiter = -1 ! -1 = no maximum
   
   vcl = 128 ! Garbage but passed to C function.
   ! Note that C structure "view" is not yet used!
   ! When created it will simply use proper memory field. Fascinating!!!
   
   ftemp=123 ! Garbage but passed to C function as a two way argument.
   
   ! Call the PSO engine:
   r = fort_pso(x,n,nb,vmin,vmax,eps,maxiter,xtemp,ftemp)
   
   !All arguments of Fortran calls are pointers -> two way direct access  
   WRITE(*,*) vcl, xtemp(1), ftemp
   
END PROGRAM imp
