/*
 *	"@(#)r_dim.c	1.1"
 */

double r_dim(a,b)
float *a, *b;
{
return( *a > *b ? *a - *b : 0);
}
