# include	<sccs.h>

SCCSID(@(#)ztack.c	7.1	2/5/81)

/*
**  LOCAL STRING CONCATENATE
**	Strings `a' and `b' are concatenated and left in an
**	internal buffer.  A pointer to that buffer is returned.
**
**	Ztack can be called recursively as:
**		ztack(ztack(ztack(w, x), y), z);
*/

char *
ztack(a, b)
register char	*a, *b;
{
	register char	*c;
	static char	buf[101];
	
	c = buf;
	
	while (*a)
		*c++ = *a++;
	while (*b)
		*c++ = *b++;
	*c = '\0';
	if (buf[100] != 0)
		syserr("ztack overflow: %s", buf);
	return (buf);
}
