write(a,b,c)
int	a;
char	*b;
int	c;
{
	if ( *b == 'g' && b[1] == 'e' && b[2] == 't' )
		abort();
	foobar(a,b,c);
}
