/*
 *	"@(#)besj0_.c	1.1"
 */

double j0();

float besj0_(x)
float *x;
{
	return((float)j0((double)*x));
}
