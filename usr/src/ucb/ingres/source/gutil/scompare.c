# include	<sccs.h>

SCCSID(@(#)scompare.c	7.1	2/5/81)

/*
**  STRING COMPARE
**
**	The strings 'a_ptr' and 'b_ptr' are compared.  Blanks are
**	ignored.  The first string may be no longer than 'a_len'
**	bytes, and the second string may be no longer than 'b_len'
**	bytes.  If either length is zero, it is taken to be very
**	long.  A null byte also terminates the scan.
**
**	Compares are based on the ascii ordering.
**
**	Shorter strings are less than longer strings.
**
**	Return value is positive one for a > b, minus one for a < b,
**	and zero for a == b.
**
**	Examples:
**		"abc" > "ab"
**		"  a bc  " == "ab  c"
**		"abc" < "abd"
*/

scompare(a_ptr, a_len, b_ptr, b_len)
char	*a_ptr;
int	a_len;
char	*b_ptr;
int	b_len;
{
	char		*ap;
	char		*bp;
	register char	a;
	char		b;
	register int	al;
	register int	bl;

	ap = a_ptr;
	bp = b_ptr;
	al = a_len;
	if (al == 0)
		al = 32767;
	bl = b_len;
	if (bl == 0)
		bl = 32767;

	while (1)
	{

		/* supress blanks in both strings */
		while ((a = *ap) == ' ' && al > 0)
		{
			al--;
			ap++;
		}
		if (al == 0)
			a = 0;
		while (*bp == ' ' && bl > 0)
		{
			bl--;
			bp++;
		}
		if (bl == 0)
			b = 0;
		else
			b = *bp;

		/* do inequality tests */
		if (a < b)
			return (-1);
		if (a > b)
			return (1);
		if (a == 0)
			return (0);

		/* go on to the next character */
		ap++;
		al--;
		bp++;
		bl--;
	}
}
