double jn();

float besjn_(n, x)
long *n; float *x;
{
	return((float)jn((int)*n, (double)*x));
}
