#include <stdio.h>
#include <math.h>

FLOATFUNCTIONTYPE gammastuff_(power)
float *power;
{
   double lg1, lg2, g1, g2;
   double b1, b2;
   float ans;
   extern int signgam;
   double lgamma();

   b1 = 0.0 - *power;
   b2 = 0.5 - *power;
   lg1 = lgamma(b1);
   lg2 = lgamma(b2);
   g1 = signgam*exp(lg1);
   g2 = signgam*exp(lg2);
   ans = g1/g2;
   RETURNFLOAT(ans);
}

double szfunc(gamma, beta, bee, rc, w, asec2cm)
float gamma;
float beta;
float bee;
float rc;
float asec2cm;
double w;
{
   double result;
/*   printf("In szfunc: gamma beta rc bee %f %f %f %f\n",
                  gamma, beta, rc, bee);
*/
   result = log(1.0 + pow(bee/rc, 2.0) + w*w);
   result *= -1.5*((gamma-1.0)/gamma)*beta;
   result += 1.0;
   result = rc*asec2cm*pow(result, gamma/(gamma-1.0));
   return result;
}

FLOATFUNCTIONTYPE integrate_sz_(num_points, gamma, beta, rc, bee, asec2cm)
int *num_points;
float *gamma, *beta, *rc, *bee;
float *asec2cm;
{
    double *weights, *ws;
    double lower_limit, upper_limit, biggest_bee, number;
    float sz;
    double szfunc();
    int i;
    weights = (double *)calloc(num_points+1, sizeof(double));
    ws = (double *)calloc(num_points+1, sizeof(double));

    sz = 0.0;
    lower_limit = 0;
    number = 2.0*(*gamma)/(3.0*(*beta)*(*gamma-1.0));
/*    printf("In integrate: Gamma beta rc bee number %f %f %f %f %f\n", 
               *gamma, *beta,
               *rc, *bee, number);
*/
    biggest_bee = sqrt(exp(number)-1.0);
/*    printf("Biggest bee is %f\n", biggest_bee);  */
    if(((*bee)/(*rc))< biggest_bee){
       upper_limit = sqrt((biggest_bee*biggest_bee)
                          -(*bee)*(*bee)/((*rc)*(*rc)));
/*       printf("In integrate: upper limit is %f\n", upper_limit);*/
       gauleg(lower_limit, upper_limit, ws, weights, *num_points);
       for(i=1;i<=*num_points;i++){
          sz +=((*(weights+i))*szfunc(*gamma, *beta, *bee, *rc, *(ws+i),
                           *asec2cm));
       }
    }
    free(weights);
    free(ws);
/*    fprintf(stderr, "in integrate_sz: %f\n", sz);  */
    RETURNFLOAT( sz);
}
