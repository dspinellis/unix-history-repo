/*
 *	"@(#)d_sign.c	1.1"
 */

double d_sign(a,b)
double *a, *b;
{
double x;
x = (*a >= 0 ? *a : - *a);
return( *b >= 0 ? x : -x);
}
