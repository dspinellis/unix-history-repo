#include "complex"

pow_zz(r,a,b)
dcomplex *r, *a, *b;
{
double logr, logi, x, y;
double log(), exp(), cos(), sin(), atan2(), cabs();

logr = log( cabs(a->dreal, a->dimag) );
logi = atan2(a->dimag, a->dreal);

x = exp( logr * b->dreal - logi * b->dimag );
y = logr * b->dimag + logi * b->dreal;

r->dreal = x * cos(y);
r->dimag = x * sin(y);
}
