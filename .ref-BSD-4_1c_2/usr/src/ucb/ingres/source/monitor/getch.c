# include	"monitor.h"
# include	<ingres.h>
# include	<aux.h>
# include	<sccs.h>

SCCSID(@(#)getch.c	7.1	2/5/81)



/*
**  GET CHARACTER
**
**	This routine is just a getchar, except it allows a pseudo-
**	EOF marker.
*/

char
getch()
{
	register char	c;

	if (GiveEof)
		c = '\0';
	else
		c = getc(Input);
	if (c < 0)
		c = '\0';

	/* deliver EOF if newline in Oneline mode */
	if (c == '\n' && Oneline)
	{
		ungetc(c, Input);
		c = '\0';
	}

	GiveEof = FALSE;
	return (c);
}
