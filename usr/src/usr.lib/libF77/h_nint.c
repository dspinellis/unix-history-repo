/*
 *	"@(#)h_nint.c	1.1"
 */

short h_nint(x)
float *x;
{
double floor();

return( (*x)>=0 ?
	floor(*x + .5) : -floor(.5 - *x) );
}
