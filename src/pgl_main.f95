PROGRAM pgl_main
! Peter's attempt to bind everything together using standard Fortran 2003/05 approach.
!
! There is nothing in this file that does any C/Fortran binding. That is left to the pgl_bind_mod module.
!
! Some of Tomasz's original code dealing with variable "vcl" had to be commented because it was non-standard
! ... or obsolete, or obsolescent ... I don't know which. Whatever the case, it looks like Tomasz realized this
! and started making changes to deal with it.

   USE pgl_bind_mod, ONLY: pgl_bind
   
   IMPLICIT NONE
   
   INTEGER, PARAMETER :: n=2
   
!   REAL, ALLOCATABLE, DIMENSION(:) :: x,vmin,vmax,xtemp
   REAL, DIMENSION(2) :: x,vmin,vmax,xtemp
   REAL :: r,eps,ftemp
   INTEGER :: j,nb,maxiter !,n,error
   
   ! example3 -> access to internals of C function through structure
!   INTEGER :: vcl
!   COMMON/v/ vcl
   
   ! Set dimension of the problem and allocate arrays:
!   n=3
!   ALLOCATE( x(n),     stat=error )
!   ALLOCATE( vmin(n),  stat=error )
!   ALLOCATE( vmax(n),  stat=error )
!   ALLOCATE( xtemp(n), stat=error )
   
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
   
!   vcl = 128 ! Garbage but passed to C function.
   ! Note that C structure "view" is not yet used!
   ! When created it will simply use proper memory field. Fascinating!!!
   
   ftemp=123 ! Garbage but passed to C function as a two way argument.
   
   ! Call the PSO engine:
   ! .. Well, actually call another Fortran function that does the C/Fortran binding:
   r = pgl_bind(x,n,nb,vmin,vmax,eps,maxiter,xtemp,ftemp)
   
   !All arguments of Fortran calls are pointers -> two way direct access
!   WRITE(*,*) vcl, xtemp(1), ftemp
   WRITE(*,*) xtemp(1:n), ftemp
   
END PROGRAM pgl_main
