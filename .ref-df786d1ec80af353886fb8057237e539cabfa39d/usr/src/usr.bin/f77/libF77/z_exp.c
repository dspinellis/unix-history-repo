/*
 *	"@(#)z_exp.c	1.1"
 */

#include "complex"

z_exp(r, z)
dcomplex *r, *z;
{
double expx;
double exp(), cos(), sin();

expx = exp(z->dreal);
r->dreal = expx * cos(z->dimag);
r->dimag = expx * sin(z->dimag);
}
