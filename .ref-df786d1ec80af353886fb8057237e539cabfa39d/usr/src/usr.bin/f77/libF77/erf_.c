/*
 *	"@(#)erf_.c	1.1"
 */

float erf_(x)
float *x;
{
double erf();

return( erf(*x) );
}
