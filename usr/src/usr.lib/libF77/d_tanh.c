/*
 *	"@(#)d_tanh.c	1.1"
 */

double d_tanh(x)
double *x;
{
double tanh();
return( tanh(*x) );
}
