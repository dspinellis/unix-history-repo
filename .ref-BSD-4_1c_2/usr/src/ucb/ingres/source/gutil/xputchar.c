# include	<stdio.h>
# include	<sccs.h>

SCCSID(@(#)xputchar.c	7.1	2/5/81)

/*
**  PUT CHARACTER
**
**	This routine just calls putchar normally, unless the character
**	to be printed is a control character, in which case the octal
**	equivalent is printed out.  Note that tab, newline, and so
**	forth are considered to be control characters.
**
**	Parameters:
**		c -- the character to print.
**
**	Returns:
**		nothing.
**
**	Side Effects:
**		none
**
**	Trace Flags:
**		none
*/



xputchar(c)
register char	c;
{
	if (c < 040 || c >= 0177)
	{
		putc('\\', stdout);
		putc(((c >> 6) & 03) | '0', stdout);
		putc(((c >> 3) & 07) | '0', stdout);
		putc((c & 07) | '0', stdout);
	}
	else
		putc(c, stdout);
}
