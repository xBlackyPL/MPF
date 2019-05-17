MODULE pgl_fun_mod

   USE, INTRINSIC :: iso_c_binding, ONLY: C_INT, C_FLOAT
   
   IMPLICIT NONE
   
   PRIVATE
   PUBLIC :: fun, cir2, ros
   
CONTAINS

   REAL(KIND=C_FLOAT) PURE FUNCTION fun() RESULT(t) BIND(C)
      IMPLICIT NONE
      t = 3.14159
   END FUNCTION fun

   REAL(KIND=C_FLOAT) PURE FUNCTION cir2(x) RESULT(t) BIND(C)
   ! Calculates area of a circle of radius x.
      IMPLICIT NONE
      REAL(KIND=C_FLOAT), INTENT(IN) :: x
      t = 3.14159*x*x
   END FUNCTION cir2

   REAL(KIND=C_FLOAT) PURE FUNCTION ros(x,n) RESULT(t) BIND(C) ! the BIND(C) is required for C/Fortran interoperability
   ! Calculates Rosenbrock function.
      IMPLICIT NONE
      INTEGER(KIND=C_INT), INTENT(IN) :: n
      REAL(KIND=C_FLOAT), DIMENSION(n), INTENT(IN) :: x
      !REAL(KIND=C_FLOAT), DIMENSION(*), INTENT(IN) :: x
      INTEGER :: j
      t = 0.0
      DO j = 1, n-1, 1
         t = t + 100.0*(x(j+1)-x(j)*x(j))**2.0 + (1.0-x(j))**2.0
      END DO
   END FUNCTION ros

END MODULE pgl_fun_mod
