#define log10e 0.43429448190325182765

double r_lg10(x)
float *x;
{
double log();

return( log10e * log(*x) );
}
