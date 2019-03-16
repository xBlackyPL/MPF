! As far as I (Peter) can tell, these functions are not used anywhere.

REAL PURE FUNCTION cir2(x) RESULT(t)
! Calculates area of a circle of radius x.
   IMPLICIT NONE
   REAL, INTENT(IN) :: x
   t = 3.14159*x*x
END FUNCTION cir2

REAL PURE FUNCTION ros(x,n) RESULT(t)
! Calculates Rosenbrock function.
   IMPLICIT NONE
   INTEGER, INTENT(IN) :: n
   REAL, INTENT(IN) :: x(n)
   INTEGER :: j
   t = 0.0
   DO j = 1, n-1, 1
      t = t + 100.0*(x(j+1)-x(j)*x(j))**2.0 + (1.0-x(j))**2.0
   END DO
END FUNCTION ros
