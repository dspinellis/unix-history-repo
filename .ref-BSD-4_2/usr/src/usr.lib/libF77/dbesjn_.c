/*
 *	"@(#)dbesjn_.c	1.1"
 */

double jn();

double dbesjn_(n, x)
long *n; double *x;
{
	return(jn((int)*n, *x));
}
