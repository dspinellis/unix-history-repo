/*
 *	"@(#)derfc_.c	1.1"
 */

double derfc_(x)
double *x;
{
double erfc();

return( erfc(*x) );
}
