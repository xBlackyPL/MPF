REAL PURE FUNCTION t_func(x,n) RESULT(t)
! Fortran target function.
! Calculates Rosenbrock function.
! Used in cfunc.c and main2.c

   IMPLICIT NONE
   INTEGER, INTENT(IN) :: n
   REAL, INTENT(IN) :: x(n)
   
   INTEGER :: j
   
   t = 0
   
   DO j = 1, n-1, 1
      t = t + 100.0*(x(j+1)-x(j)*x(j))**2.0 + (1.0-x(j))**2.0
   END DO

END FUNCTION t_func
