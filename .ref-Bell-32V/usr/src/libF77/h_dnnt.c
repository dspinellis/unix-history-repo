short h_dnnt(x)
double *x;
{
return( (*x)>=0 ?
	(short) (*x + .5) : (short) (*x - .5) );
}
