__length(s)
char	*s;
{
	register int	l;
	register char	*p;

	p = s;
	l = 0;
	while (*p++)
		l++;
	return(l);
}
