double jn();

double dbesjn_(n, x)
long *n; double *x;
{
	return(jn((int)*n, *x));
}
