/*
 *	"@(#)z_abs.c	1.1"
 */

#include "complex"

double z_abs(z)
dcomplex *z;
{
double cabs();

return( cabs( z->dreal, z->dimag ) );
}
