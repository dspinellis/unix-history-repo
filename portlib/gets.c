/**
 **	read a string
 **/

gets(s)
char	*s;
{
	register char	*p;
	register char	c;

	p = s;
	while (c = getchar())
	{
		if (c == '\n')
			break;
		*p++ = c;
	}
	if (c == '\0')
		return (0);
	*p = '\0';
	return (s);
}
