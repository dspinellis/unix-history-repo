/*
 *	"@(#)d_int.c	1.1"
 */

double d_int(x)
double *x;
{
double floor();

return( (*x >= 0) ? floor(*x) : -floor(- *x) );
}
