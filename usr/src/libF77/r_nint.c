double r_nint(x)
float *x;
{
return( (*x)>=0 ?
	(long int) (*x + .5) : (long int) (*x - .5) );
}
