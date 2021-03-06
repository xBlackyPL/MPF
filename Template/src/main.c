#include <stdio.h>
#include <math.h>
#include "pso.h"
#include <stdlib.h>
#include <omp.h>
#include <unistd.h>

//Ugly stuff but necessary to have "through structure" fortran interoperability 
//here unused
struct view v_;

//example function - genaralized Rosenbrock fuction.
float grosen(float *x,int n)
{
    float f;
    int j;

    f=0.0f;
    for (j=0;j<n-1;j++)
    {
        f=f+100.0f*powf(x[j+1]-x[j]*x[j],2.0f) + powf(1.0f-x[j],2.0f);
    }	

    return f;
}

int main()
{
    int j;
    float *x,*min,*max;
    float eps,*xtemp,ftemp,f;
    int maxiter;
    int n,nb,flag,nt;


    n=100; //dimensions (a lot of)
    nb=128; // number of particles 

    //mem init
    x=(float *) calloc(n,sizeof(float));
    xtemp=(float *) calloc(n,sizeof(float));
    min=(float *) calloc(n,sizeof(float));
    max=(float *) calloc(n,sizeof(float));

    //ranges used ONLY for particle initialisation
    for (j=0;j<n;j++)
    {
        min[j]=0.5;
        max[j]=1.5;
    }

    //maximum precision (used for stopping criterion) 
    eps=0.00001;
    //maximum number of iterations (-1 - no maximum)
    maxiter=-1;

    //regular parallel call (based on OMP_NUM_THREADS) uncomment it for regular application
    //f=pso(x,n,&grosen,nb,min,max,eps,&maxiter,&xtemp[0],&ftemp);

    //x -> results 
    //n -> number of dimensions
    //grosen -> function
    //min,max -> see line 38
    //maxiter,eps -> see line 45 and 47
    //xtemp, ftem -> temporal best solution and corresponding target function value 


    // example of using OMP threads for external control of function 

#pragma omp parallel
    nt=omp_get_num_threads(); // store value from OMP_NUM_THREADS 

    omp_set_nested(1); //turns on nested OMP (threads within threads) 
    omp_set_num_threads(2); //two threads will be started in the next parallel section
    flag=0; //control flag (has function exited?)
#pragma omp parallel
    {
        if(omp_get_thread_num()==0) //thread 0. Does real work using nt number of threads
        {
            omp_set_num_threads(nt);
            f=pso(x,n,&grosen,nb,min,max,eps,&maxiter,&xtemp[0],&ftemp);
            flag=1; // function has exited (leave parallel section)
        }
        else // control part (thread 1)
        {
            while(1) //every 1 second dump temporal information about solution.
            {
                sleep(1);
                for (j=0;j<n;j++)
                {
                    fprintf(stdout,"%f ",xtemp[j]);
                }
                fprintf(stdout,"%f\n",ftemp);

                if (ftemp<0.01) //useful trick. Use maxiter to force stop the fuction when some previously unknown criterion has been fulfilled
                    maxiter=1;

                if (flag==1) // function exited -> leave parallel section. 
                    break;
            }
        }	
    }

    //example ends here. Comment it out for regular application. 

    //dump final results 
    for (j=0;j<n;j++)
    {
        fprintf(stdout,"%f ",x[j]);
    }
    fprintf(stdout,"%f\n",f);


    return 0;
}
