#define log10e 0.43429448190325182765

double d_lg10(x)
double *x;
{
double log();

return( log10e * log(*x) );
}
