/*
 *	"@(#)d_abs.c	1.1"
 */

double d_abs(x)
double *x;
{
if(*x >= 0)
	return(*x);
return(- *x);
}
