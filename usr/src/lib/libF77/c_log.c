#include "complex"

c_log(r, z)
complex *r, *z;
{
double log(), cabs(), atan2();

r->imag = atan2(z->imag, z->real);
r->real = log( cabs(z->real, z->imag) );
}
