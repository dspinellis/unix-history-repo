/**
 **	put a string to the standard output
 **/

puts(s)
char	*s;
{
	register char	c;
	register char	*p;

	for (p = s; c = *p++; )
		putchar(c);
	putchar('\n');
	return (s);
}
