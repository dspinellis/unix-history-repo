int	optr;

char	obuf[512];

int	nflush;

put(string,n)
	char	*string;
{
	int	i;
	char	*o;

/*printf("%c %d\n",*string,n);/*DEBUG*/

	string--;

	if((i = optr + n - 512) >= 0) {
		n -= i;
		o = &obuf[optr] -1;
		while(--n >= 0)
			*++o = *++string;
		optr = 512;
		flsh();
		n = i;
	}

	o = &obuf[optr] - 1;
	optr += n;

	while(--n >= 0) {
		*++o = *++string;
	}
	return(0);
}

flsh()
{

	if(optr <= 0)	return(optr);

	nflush++;
	if(write(1,obuf,optr) != optr)
		return(-1);
	optr = 0;
	return(0);
}

