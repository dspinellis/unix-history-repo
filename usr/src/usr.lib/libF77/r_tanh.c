/*
 *	"@(#)r_tanh.c	1.1"
 */

double r_tanh(x)
float *x;
{
double tanh();
return( tanh(*x) );
}
