#include "complex"

z_sin(r, z)
dcomplex *r, *z;
{
double sin(), cos(), sinh(), cosh();

r->dreal = sin(z->dreal) * cosh(z->dimag);
r->dimag = cos(z->dreal) * sinh(z->dimag);
}
