# include	"monitor.h"
# include	<ingres.h>
# include	<aux.h>
#include	<sccs.h>

SCCSID(@(#)branch.c	7.1	2/5/81)



/*
**  BRANCH
**
**	The "filename" following the \b op must match the "filename"
**	which follows some \k command somewhere in this same file.
**	The input pointer is placed at that point if possible.  If
**	the label does not exist, an error is printed and the next
**	character read is an EOF.
**
**	Trace Flags:
**		33
*/

branch()
{
	register char	c;
	register int	i;
	extern char	getch();

#	ifdef xMTR2
	if (tTf(33, -1))
		printf(">>branch: ");
#	endif

	/* see if conditional */
	while ((c = getch()) > 0)
		if (c != ' ' && c != '\t')
			break;
	if (c == '?')
	{
		/* got a conditional; evaluate it */
		Oneline = TRUE;
		macinit(getch, 0, 0);
		i = expr();

		if (i <= 0)
		{
			/* no branch */
#			ifdef xMTR2
			if (tTf(33, 0))
				printf("no branch\n");
#			endif
			getfilenm();
			return;
		}
	}
	else
	{
		ungetc(c, Input);
	}

	/* get the target label */
	if (branchto(getfilenm()) == 0)
		if (branchto(macro("{default}")) == 0)
		{
			GiveEof = TRUE;
			printf("Cannot branch\n");
		}
	return;
}


branchto(label)
char	*label;
{
	char		target[100];
	register char	c;

	smove(label, target);
	if (rewind(Input))
	{
		printf("Cannot branch on a terminal\n");
		return (1);
	}

	/* search for the label */
	while ((c = getch()) > 0)
	{
		if (c != '\\')
			continue;
		if (getescape(0) != C_MARK)
			continue;
		if (sequal(getfilenm(), target))
			return;
	}
}
