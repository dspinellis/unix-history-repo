/**
 **	put a single character to the standard output
 **/

putchar(c)
char	c;
{
	extern int	cout;

	cputc(c, cout);
}
