/*
 *	"@(#)besyn_.c	1.1"
 */

double yn();

float besyn_(n, x)
long *n; float *x;
{
	return((float)yn((int)*n, (double)*x));
}
