/*
 *	"@(#)c_abs.c	1.1"
 */

#include "complex"

float c_abs(z)
complex *z;
{
double cabs();

return( cabs( z->real, z->imag ) );
}
