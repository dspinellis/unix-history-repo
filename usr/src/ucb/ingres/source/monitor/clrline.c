# include	"monitor.h"
# include	<ingres.h>
# include	<aux.h>
# include	<sccs.h>

SCCSID(@(#)clrline.c	7.1	2/5/81)



/*
**  Clear Input Line
**
**	This routine removes the newline following a monitor command
**	(such as \t, \g,  etc.)  Any other characters are processed.
**	Hence, \t\g\t will work.  It also maintains the
**	Newline flag on command lines.  It will make certain that
**	the current line in the query buffer ends with a newline.
**
**	The flag 'noprompt' if will disable the prompt character if set.
**	Otherwise, it is automatically printed out.
**
**	Uses trace flag 8
*/

clrline(noprompt)
int	noprompt;
{
	register char	c;

	if (!Newline)
		putc('\n', Qryiop);
	Newline = TRUE;
	/* if char following is a newline, throw it away */
	c = getch();
	Prompt = c == '\n';
	if (!Prompt)
	{
		ungetc(c, Input);
	}
	else
	{
		if (!noprompt)
			prompt(0);
	}
	return;
}
