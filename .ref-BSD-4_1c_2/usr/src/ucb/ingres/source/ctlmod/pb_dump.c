# include	"pipes.h"
# include	<sccs.h>

SCCSID(@(#)pb_dump.c	7.1	2/5/81)

/*
**  PB_DUMP -- dump pipe buffer for debugging
**
**	Parameters:
**		ppb -- pointer to the structure to dump.
**		full -- if set, dump everything, else just
**			dump the header.
**
**	Returns:
**		none
**
**	Side Effects:
**		none
**
**	Trace Flags:
**		none
*/

pb_dump(ppb, full)
register pb_t	*ppb;
int		full;
{
	printf("PB @ %x:\n", ppb);
	printf("\t    st %4d  proc %4d  resp %4d  from %4d\n",
	    ppb->pb_st, ppb->pb_proc, ppb->pb_resp, ppb->pb_from);
	printf("\t    type %2d  stat %4o  nused %3d  nleft %3d",
	    ppb->pb_type, ppb->pb_stat, ppb->pb_nused, ppb->pb_nleft);
	if (full)
	{
		register int	i;
		register char	*p;
		register char	c;

		p = ppb->pb_data;
		for (i = 0; i < ppb->pb_nused; i++)
		{
			c = *p++;
			if (i % 10 == 0)
				printf("\n\t%3d:", i);
			putchar(' ');
			if (c >= 040 && c < 0177)
				printf(" %c  ", c);
			else
				xputchar(c);
		}
	}
	putchar('\n');
}
