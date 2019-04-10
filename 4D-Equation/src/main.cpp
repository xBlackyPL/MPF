#include <iostream>
#include <random>
#include <omp.h>
#include <unistd.h>

#include "blaze/math/UniformMatrix.h"
extern "C" {
    #include "pso.h"
}

using namespace blaze;

const unsigned int dimensions = 4;
using Guess = std::array<float, dimensions>;

template <typename T>
Guess normalize_random_values(T vec) {
    float a=vec[0];
    float b=vec[1];
    float c=vec[2];
    float d=vec[3];
    float sqrt_length = sqrt(a*a + b*b + c*c + d*d);
    a/=sqrt_length;
    b/=sqrt_length;
    c/=sqrt_length;
    d/=sqrt_length;

    Guess result{a,b,c,d};
    return result;
}

DynamicMatrix<float> generate_r(Guess guess) {
    float a = guess[0];
    float b = guess[1];
    float c = guess[2];
    float d = guess[3];

    return DynamicMatrix<float>{
    {a*a+b*b-c*c-d*d, -2*a*d+2*b*c, 2*a*c+2*b*d},
    {2*a*d+2*b*c, a*a-b*b+c*c-d*d, -2*a*b+2*c*d},
    {-2*a*c+2*b*d, 2*a*b+2*c*d, a*a-b*b-c*c+d*d}};
}

float equation (float* x, int n)
{
    std::vector<float> raw_guess {x[0], x[1], x[2], x[3]};
    auto guess = normalize_random_values(raw_guess);
    float a = guess[0];
    float b = guess[1];
    float c = guess[2];
    float d = guess[3];
    DynamicMatrix<double> ingred_a {{1.1880, 0.0787, 0.7829}, {0.0787, 1.1128, -0.0244}, {0.7829, -0.0244, 1.3956}};
    DynamicMatrix<double> ingred_b {{1.0269, 0.1090, 0.6547}, {0.1090, 1.0386, -0.2852}, {0.6547, -0.2852, 1.6309}};

    DynamicMatrix<double> r   {{a*a+b*b-c*c-d*d, -2*a*d+2*b*c, 2*a*c+2*b*d},
                              {2*a*d+2*b*c, a*a-b*b+c*c-d*d, -2*a*b+2*c*d},
                              {-2*a*c+2*b*d, 2*a*b+2*c*d, a*a-b*b-c*c+d*d}};

    DynamicMatrix<double> r_t {{a*a+b*b-c*c-d*d, 2*a*d+2*b*c, -2*a*c+2*b*d},
                              {-2*a*d+2*b*c, a*a-b*b+c*c-d*d, 2*a*b+2*c*d}, 
                              {2*a*c+2*b*d, -2*a*b+2*c*d, a*a-b*b-c*c+d*d}};

    DynamicMatrix<double> res1 = r * ingred_a;
    DynamicMatrix<double> res2 = res1 * r_t;
    DynamicMatrix<double> res3 = res2 - ingred_b;

    float sum_of_squares = 0;
    for( size_t i=0UL; i< res3.rows(); ++i ) {
        for( size_t j=0UL; j < res3.columns(); ++j ) {
            sum_of_squares += res3(i,j) * res3(i,j);
        }
    }
    return sum_of_squares;
}

int main(int argc, char const* argv[]) {
    int j;
    float *x,*min,*max;
    float *xtemp,ftemp,f;
    int maxiter;
    int n,nb,flag,nt;
    float eps = 0.000001f;
    n=4;
    nb=128; 

    x=(float *) calloc(n,sizeof(float));
    xtemp=(float *) calloc(n,sizeof(float));
    min=(float *) calloc(n,sizeof(float));
    max=(float *) calloc(n,sizeof(float));

    for (j=0;j<n;j++)
    {
        min[j]=-1;
        max[j]=1;
    }

    maxiter=-1;

    #pragma omp parallel
    nt=omp_get_num_threads(); // store value from OMP_NUM_THREADS 

    omp_set_nested(1); //turns on nested OMP (threads within threads) 
    omp_set_num_threads(2); //two threads will be started in the next parallel section
    flag=0; //control flag (has function exited?)
    #pragma omp parallel
    {
        if(omp_get_thread_num()==0)
        {
            omp_set_num_threads(nt);
           
               f=pso(x,n,&equation,nb,min,max,eps,&maxiter,&xtemp[0],&ftemp);
            
            flag=1;
        }
        else // control part (thread 1)
        {
            while(1) //every 1 second dump temporal information about solution.
            {
                sleep(1);
                if (ftemp<0.01) //useful trick. Use maxiter to force stop the fuction when some previously unknown criterion has been fulfilled
                    maxiter=1;
                
                if (flag==1) // function exited -> leave parallel section. 
                    break;
            }
        }	
    }

    double e=sqrt(x[0]*x[0]+x[1]*x[1]+x[2]*x[2]+x[3]*x[3]);
    for (j=0;j<n;j++)
    {
        fprintf(stdout,"%f ",x[j]);
    }
    fprintf(stdout,"\t %f \t%f \n", e, f);
    return 0;
}
