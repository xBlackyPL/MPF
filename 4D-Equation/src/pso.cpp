#include "pso.hpp"
#include <float.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_rng.h>
#include <math.h>
#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>

float pso(float *x, int n, float (*ptfunc)(float *x, int n), int nb, float *min,
          float *max, float eps, int *maxiter, float *xtemp, float *ftemp) {
    struct psobird *b;
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

    gettimeofday(&t, NULL);
    sprintf(pomm, "%d", (int)(t.tv_sec / 1000 + t.tv_usec));
    setenv("GSL_RNG_SEED", pomm, 1);
    gsl_rng_env_setup();
    T = gsl_rng_default;
    R = gsl_rng_alloc(T);

    srand(t.tv_sec / 1000 + t.tv_usec);
    for (i = 0; i < nb; i++) {
        b[i].ssigma = 1;
        for (j = 0; j < n; j++) {
            b[i].pos[j] =
                min[j] + (max[j] - min[j]) * (float)rand() / (float)RAND_MAX;
        }

        b[i].local = ptfunc(b[i].pos, n);

        for (j = 0; j < n; j++) {
            b[i].pbest[j] = b[i].pos[j];
        }
        b[i].pbesterror = b[i].local;
    }

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
                if (gbesterror > b[i].pbesterror) {
                    cl = 0;
                    gbesterror = b[i].pbesterror;
                    for (j = 0; j < n; j++) {
                        gbest[j] = b[i].pos[j];
                        b[i].pbest[j] = min[j] + (max[j] - min[j]) *
                                                     (float)rand() /
                                                     (float)RAND_MAX;
                        b[i].pos[j] = b[i].pbest[j];  //-
                        xtemp[j] = gbest[j];
                        *ftemp = gbesterror;
                    }
                    b[i].pbesterror = ptfunc(b[i].pbest, n);
                    b[i].local = b[i].pbesterror;
                }
            }

            if (b[i].ssigma < (eps * (float)n)) {
                cl++;
                for (j = 0; j < n; j++) {
                    b[i].pbest[j] = min[j] + (max[j] - min[j]) * (float)rand() /
                                                 (float)RAND_MAX;
                    b[i].pos[j] = b[i].pbest[j];  //-
                }
                b[i].pbesterror = ptfunc(b[i].pbest, n);
                b[i].local = b[i].pbesterror;  //-
            }
        }
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
            b[i].local = ptfunc(b[i].pos, n);
        }

        if (iter >= *maxiter && *maxiter > 0) break;
    }

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