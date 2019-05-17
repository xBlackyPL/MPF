// Peter's changes to Tomasz's original are marked with "PGL" and summarized
// below:
//
// The target function has changed from (*ptfunc)(float *x,int n) to
// (*ptfunc)(float *x,int *n)
// ("int n" to "int *n") so that only pointers are passed to the Fortran target
// function. This has to do with Fortran being pass-by-reference (and C pointers
// being essentially references).
//
// Making that change meant having to replace "n" with "&n" in various places
// so that a pointer is passed to the Fortran target function.
//
// The other changes here have to do with changes explained in pgl_main.

#include <stdio.h>
// #include "pso.h"
#include <float.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_rng.h>
#include <math.h>
#include <omp.h>
#include <stdlib.h>
#include <sys/time.h>
#include "pgl_pso.hpp"

void psodummy(void (*ptfunc)()) { ptfunc(); }

// x -> results
// n -> number of dimensions
// ptfunc -> function
// min,max -> ranges used ONLY for particle initialisation
// maxiter -> maximum number of iterations (-1 = no maximum)
// eps -> maximum precision (used for stopping criterion)
// xtemp, ftem -> temporal best solution and corresponding target function value

// float pso (float *x,int n,float (*ptfunc)(float *x,int n),int nb,float
// *min,float *max,float eps,int *maxiter,float *xtemp,float *ftemp)
float pgl_pso(float *x, int n, float (*ptfunc)(float *x, int *n), int nb,
              float *min, float *max, float eps, int *maxiter, float *xtemp,
              float *ftemp)  // PGL
{
    // PGL added
    printf("%s\n", "Hello from pgl_pso");
    //*xtemp = *x;
    //*ftemp = ptfunc(x,&n);
    // return 0.0;
    //}

    struct psobird *b;
    // extern struct view v_; //structure used for communication with fortran //
    // PGL commented
    struct timeval t;
    int i, j, iter, cl;
    char pomm[80];
    gsl_rng *R;
    const gsl_rng_type *T;
    float *gbest, gbesterror;
    float sigma;

    b = (struct psobird *)calloc(nb, sizeof(struct psobird));

    for (i = 0; i < nb; i++) {
        b[i].number = i;
        b[i].pos = (float *)calloc(n, sizeof(float));
        b[i].pbest = (float *)calloc(n, sizeof(float));
    }

    gbest = (float *)calloc(n, sizeof(float));

    // fprintf(stderr,"%Ld\n",v_.cl);
    // exit(0);

    gettimeofday(&t, NULL);
    sprintf(pomm, "%d", (int)(t.tv_sec / 1000 + t.tv_usec));
    setenv("GSL_RNG_SEED", pomm, 1);
    gsl_rng_env_setup();
    T = gsl_rng_default;
    R = gsl_rng_alloc(T);

    // init
    srand(t.tv_sec / 1000 + t.tv_usec);
    for (i = 0; i < nb; i++) {
        b[i].ssigma = 1;
        for (j = 0; j < n; j++) {
            b[i].pos[j] =
                min[j] + (max[j] - min[j]) * (float)rand() / (float)RAND_MAX;
        }

        b[i].local = ptfunc(b[i].pos, &n);  // PGL added &

        for (j = 0; j < n; j++) {
            b[i].pbest[j] = b[i].pos[j];
        }
        b[i].pbesterror = b[i].local;

    }  // end init

    for (j = 0; j < n; j++) {
        gbest[j] = b[0].pos[j];
    }

    gbesterror = b[0].pbesterror;

    for (i = 1; i < nb; i++) {
        if (gbesterror > b[i].pbesterror) {
            gbesterror = b[i].pbesterror;
            for (j = 0; j < n; j++) {
                gbest[j] = b[i].pos[j];
            }
        }
    }

    iter = 0;
    cl = 0;
    while (1) {
        iter++;
        for (i = 0; i < nb; i++) {
            if (b[i].local < b[i].pbesterror) {
                b[i].pbesterror = b[i].local;
                for (j = 0; j < n; j++) {
                    b[i].pbest[j] = b[i].pos[j];
                }
                // echo
                /*
                fprintf(stderr,"%06d ",iter);
                for (j=0;j<n;j++)
                {
                        fprintf (stderr,"%f ",b[i].pos[j]);
                }
                fprintf(stderr,"%f %f\n",b[i].local,b[i].pbesterror);
                */
                //
                if (gbesterror > b[i].pbesterror) {
                    // fprintf(stderr,"%d\n",cl);
                    cl = 0;
                    gbesterror = b[i].pbesterror;
                    for (j = 0; j < n; j++) {
                        gbest[j] = b[i].pos[j];
                        b[i].pbest[j] = min[j] + (max[j] - min[j]) *
                                                     (float)rand() /
                                                     (float)2;
                        b[i].pos[j] = b[i].pbest[j];  //-
                        xtemp[j] = gbest[j];
                        *ftemp = gbesterror;
                    }
                    b[i].pbesterror = ptfunc(b[i].pbest, &n);  // PGL added &
                    b[i].local = b[i].pbesterror;              //-
                                                               // echo
                    /*
                    fprintf(stderr,"%06d ",iter);
                    for (j=0;j<n;j++)
                    {
                            fprintf (stderr,"%f ",gbest[j]);
                    }
                    fprintf(stderr,"%f\n",gbesterror);
                    */
                    // echo
                }
            }

            if (b[i].ssigma < (eps * (float)n)) {
                cl++;
                for (j = 0; j < n; j++) {
                    b[i].pbest[j] = min[j] + (max[j] - min[j]) * (float)rand() /
                                                 (float)RAND_MAX;
                    b[i].pos[j] = b[i].pbest[j];  //-
                }
                b[i].pbesterror = ptfunc(b[i].pbest, &n);  // PGL added &
                b[i].local = b[i].pbesterror;              //-
            }
        }
        // value cl copied to stucture "view" to be virtualy used in Fortran
        // v_.cl=cl; // PGL commented
        if (cl > 0.5 * nb) {
            break;
        }

#pragma omp parallel for private(i, j, sigma)
        for (i = 0; i < nb; i++) {
            b[i].ssigma = 0.0;
            for (j = 0; j < n; j++) {
                sigma = fabs(b[i].pbest[j] - gbest[j]);
                b[i].ssigma += sigma;
                b[i].pos[j] = ((b[i].pbest[j] + gbest[j]) / 2.0) +
                              gsl_ran_gaussian(R, sigma);
            }
            b[i].local = ptfunc(b[i].pos, &n);  // PGL added &
        }

        if (iter >= *maxiter && *maxiter > 0) break;
    }  // while

    // echo
    for (i = 0; i < nb; i++) {
        fprintf(stderr, "%06d ", i);
        for (j = 0; j < n; j++) {
            fprintf(stderr, "%f ", b[i].pbest[j]);
        }
        fprintf(stderr, "%f\n", b[i].pbesterror);
    }

    fprintf(stderr, "%06d ", iter);
    for (j = 0; j < n; j++) {
        fprintf(stderr, "%f ", gbest[j]);
    }
    fprintf(stderr, "%f\n", gbesterror);
    //

    for (j = 0; j < n; j++) {
        x[j] = gbest[j];
    }

    for (i = 0; i < nb; i++) {
        free(b[i].pos);
        free(b[i].pbest);
    }
    free(b);

    free(gbest);

    return gbesterror;
}
