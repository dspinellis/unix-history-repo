/*
 *	"@(#)dbesyn_.c	1.1"
 */

double yn();

double dbesyn_(n, x)
long *n; double *x;
{
	return(yn((int)*n, *x));
}
