__prtint(pobj, buf, base, signed, f, upper)
int	*pobj;
char	*buf;
int	base;
int	signed;
int	upper;
char	*(*f)();
{
	char		digs[15];
	register char	*dp;
	register int	k;
	register char	*p;

	dp = (*f)(pobj, &buf, base, signed, digs);

	if (dp == digs)
		*dp++ = 0;
	p = buf;
	while (dp != digs)
	{
		k = *--dp;
		if (k < 10)
			k =+ '0';
		else
			k =+ upper ? 'A' : 'a';
		*p++ = k;
	}
	*p = 0;
	return (p);
}


__prtshort(pobj, pbuf, base, signed, digs)
int	*pobj;
char	**pbuf;
int	base;
int	signed;
char	*digs;
{
	extern int	ldivr;
	register int	n;
	register char	*p;

	p = digs;
	n = *pobj;
	if (signed && n < 0)
	{
		n = -n;
		*(*pbuf)++ = '-';
	}
	while (n != 0)
	{
		n = ldiv(0, n, base);
		*p++ = ldivr;
	}
	return (p);
}
