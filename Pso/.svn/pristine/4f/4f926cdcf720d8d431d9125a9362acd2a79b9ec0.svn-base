#include "pso.h"

//fortran target function
extern float t_func_(float *x,int *pn);

//C wrapper for fortran function (note variable instead of pointer)
//c_ means that we want to use fortan function in C
float c_t_func(float *x,int n);

//Library function "pso" wrapper
//fort_ means that we intend to use C function in fortran (note wrapped pointers)  
float fort_pso_(float *x,int *pn,int *pnb, float *min, float *max, float *peps, int *maxiter, float *xtemp, float *ftemp);



float c_t_func(float *x,int n)
{
	return t_func_(&x[0],&n);
}

float fort_pso_(float *x,int *pn,int *pnb, float *min, float *max, float *peps, int *maxiter, float *xtemp, float *ftemp)
{
int n,nb;
float eps;

n=*pn;
nb=*pnb;
eps=*peps;

return pso(x,n,&c_t_func,nb,min,max,eps,maxiter,xtemp,ftemp); 
}
