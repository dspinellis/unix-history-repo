/*
 *	"@(#)d_nint.c	1.1"
 */

double d_nint(x)
double *x;
{
double floor();

return( (*x)>=0 ?
	floor(*x + .5) : -floor(.5 - *x) );
}
