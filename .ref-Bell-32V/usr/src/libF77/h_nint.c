short h_nint(x)
float *x;
{
return( (*x)>=0 ?
	(short) (*x + .5) : (short) (*x - .5) );
}
