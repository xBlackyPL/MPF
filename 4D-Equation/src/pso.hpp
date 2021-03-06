#ifndef PSO_H
#define PSO_H

float pso(float *x, int n, float (*ptfunc)(float *x, int n), int nb, float *min,
          float *max, float eps, int *maxiter, float *xtemp, float *ftemp);

struct psobird {
    int number;
    int up;
    float *pos;
    float *v;
    float *pbest;
    float *dpos;
    float pbesterror;
    float pworsterror;
    float groupbesterror;
    float *groupbest;
    float *pbestever;
    float pbesterrorever;
    float local;
    float ssigma;
};

#endif
