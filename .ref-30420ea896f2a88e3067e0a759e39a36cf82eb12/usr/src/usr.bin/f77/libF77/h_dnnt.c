/*
 *	"@(#)h_dnnt.c	1.1"
 */

short h_dnnt(x)
double *x;
{
double floor();

return( (*x)>=0 ?
	floor(*x + .5) : -floor(.5 - *x) );
}
