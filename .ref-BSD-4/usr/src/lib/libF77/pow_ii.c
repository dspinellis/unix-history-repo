long int pow_ii(ap, bp)
long int *ap, *bp;
{
long int pow, x, n;

pow = 1;
x = *ap;
n = *bp;

if(n < 0)
	{ }
else if(n > 0)
	for( ; ; )
		{
		if(n & 01)
			pow *= x;
		if(n >>= 1)
			x *= x;
		else
			break;
		}
return(pow);
}
