/*
 *	"@(#)r_cnjg.c	1.1"
 */

#include "complex"

r_cnjg(r, z)
complex *r, *z;
{
r->real = z->real;
r->imag = - z->imag;
}
