MODULE pgl_bind_mod
! Does the C/Fortran binding required to call the pso engine (well, the wrapper to it anyway).

   ! Use of the iso_c_binding module is the Fortran 2003 standard way to do C/Fortran interoperability.
   USE, INTRINSIC :: iso_c_binding, ONLY: C_INT, C_FLOAT

   IMPLICIT NONE
   
   PRIVATE
   PUBLIC :: pgl_bind
   
   ! Here we must define an interface to the C function.
   !
   ! Note that I don't use the pgl_pso function directly but wrap it with another C function called pgl_wrap.
   ! I do this in case I want to change the prototype (input/output parameters) for pgl_bind,
   ! e.g. pass in a derived type object (structure) instead of a long string of parameters.
   !
   ! I could avoid pgl_wrap and call pgl_pso directly but 1) I want to keep pgl_bind_mod as small and simple
   ! to understand as possible and 2) I want to make minimal changes to Tomasz's original pso function.
   !
   ! The idea of the pgl_wrap function is therefore to do any parameter translation between
   ! the prototype for pgl_bind and the prototype for pgl_pso, instead of having those translations
   ! in the pgl_bind function.
   !
   ! I also decided to have the Fortran target function defined as late in the game as possible,
   ! so that is done in pgl_wrap. I don't know if that is sensible, but it made coding easier.
   ! This means that the first stage of the C/Fortran binding happens in this module and
   ! the second stage of the C/Fortran binding happens in the pgl_wrap.cpp file. I like it that way.
   INTERFACE
      REAL(KIND=C_FLOAT) FUNCTION pgl_wrap(x,n,nb,vmin,vmax,eps,maxiter,xtemp,ftemp) bind(C)
         USE, INTRINSIC :: iso_c_binding, ONLY: C_INT, C_FLOAT
         REAL(KIND=C_FLOAT), DIMENSION(*), INTENT(IN) :: x,vmin,vmax
         REAL(KIND=C_FLOAT), DIMENSION(*), INTENT(OUT) :: xtemp
         REAL(KIND=C_FLOAT), VALUE, INTENT(IN) :: eps
         REAL(KIND=C_FLOAT), INTENT(OUT) :: ftemp
         ! The VALUE attribute must be present for C parameters like "int n" (non-pointers):
         INTEGER(KIND=C_INT), VALUE, INTENT(IN) :: n,nb
         ! The VALUE attribute must be absent for C parameters like "int *maxiter" (pointers):
         INTEGER(KIND=C_INT), INTENT(IN) :: maxiter
      END FUNCTION pgl_wrap
   END INTERFACE
   
CONTAINS

   REAL(C_FLOAT) FUNCTION pgl_bind(x,n,nb,vmin,vmax,eps,maxiter,xtemp,ftemp) RESULT(r)
      IMPLICIT NONE
      REAL(KIND=C_FLOAT), DIMENSION(:), INTENT(IN) :: x,vmin,vmax
      REAL(KIND=C_FLOAT), DIMENSION(:), INTENT(OUT) :: xtemp
      REAL(KIND=C_FLOAT), INTENT(IN) :: eps
      REAL(KIND=C_FLOAT), INTENT(OUT) :: ftemp
      INTEGER(KIND=C_INT), INTENT(IN) :: n,nb
      INTEGER(KIND=C_INT), INTENT(IN) :: maxiter
      r = pgl_wrap(x,n,nb,vmin,vmax,eps,maxiter,xtemp,ftemp)
   END FUNCTION pgl_bind
   
END MODULE pgl_bind_mod
