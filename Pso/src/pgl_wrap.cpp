// Does the C/Fortran binding required to tell the pso engine which Fortran
// target function to use.

#include <stdio.h>  // for printf
#include "pgl_pso.hpp"

// Fortran target function:
// (the extern "C" is required for C/Fortran interoperability)
extern float ros(float *x, int *n);  // all the parameters MUST be pointers

// Wrap Tomasz's pso function.
// Currently this function passes the input parameters directly into the pgl_pso
// function (as well as specifying the Fortran target function to minimize). If
// the prototype for pgl_bind changes then I'd want to make changes here.
extern float pgl_wrap(float *x, int n, int nb, float *min, float *max,
                      float eps, int *maxiter, float *xtemp, float *ftemp) {
    // Call the pso engine:
    return pgl_pso(x, n, ros, nb, min, max, eps, maxiter, xtemp, ftemp);
}
