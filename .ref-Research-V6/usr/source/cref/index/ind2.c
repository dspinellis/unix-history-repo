int	optr;

char	obuf[512];

int	nflush;

put(string,n)
	char	*string;
{
	extern	utmp;
	int	i;
	char	*o;

/*printf("%d %c %d\n",fil,*string,n);/*DEBUG*/

	string--;

	if((i = optr + n - 512) >= 0) {
		n =- i;
		o = &obuf[optr] -1;
		while(--n >= 0)
			*++o = *++string;
		optr = 512;
		flsh(1);
		n = i;
	}

	o = &obuf[optr] - 1;
	optr =+ n;

	while(--n >= 0) {
		*++o = *++string;
	}
	return(0);
}

flsh(fil)
{
	extern	tp[],utmp;

	if(optr <= 0)	return(optr);

	nflush++;
	if(write(tp[0],obuf,optr) != optr)
		return(-1);
	optr = 0;
	return(0);
}

