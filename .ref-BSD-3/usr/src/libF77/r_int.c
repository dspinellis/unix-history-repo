double r_int(x)
float *x;
{
double floor();

return( (*x>0) ? floor(*x) : -floor(- *x) );
}
