double d_nint(x)
double *x;
{
return( (*x)>=0 ?
	(long int) (*x + .5) : (long int) (*x - .5) );
}
