/**
 **	generate temporary file name
 **/

int	__tmpnumber	0;

tmpnam(s)
char	*s;
{
	printf(-1, s, "pl%d%c", getpid(), 'A' + __tmpnumber++);
	return (s);
}
