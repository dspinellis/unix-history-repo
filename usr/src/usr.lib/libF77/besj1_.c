/*
 *	"@(#)besj1_.c	1.1"
 */

double j1();

float besj1_(x)
float *x;
{
	return((float)j1((double)*x));
}
