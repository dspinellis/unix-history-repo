# include	<ingres.h>
# include	<access.h>
# include	<sccs.h>

SCCSID(@(#)tup_len.c	7.1	2/5/81)

/*
**	Tup_len finds the amount of space occupied by the
**	tuple specified by "tid"
*/

tup_len(tid)
TID	*tid;
{
	register short	*lp;
	register int	nextoff, off;
	int		lineoff, i;


	/* point to line number table */
	lp = (short *) Acc_head->linetab;

	/* find offset for tid */
	lineoff = lp[-(tid->line_id & I1MASK)];

	/* assume next line number follows lineoff */
	nextoff = lp[-Acc_head->nxtlino];

	/* look for the line offset following lineoff */
	for (i = 0; i < Acc_head->nxtlino; i++)
	{
		off = *lp--;

		if (off <= lineoff)
			continue;

		if (off < nextoff)
			nextoff = off;
	}
#	ifdef xATR3
	if (tTf(27, 8))
		printf("tup_len ret %d\n", nextoff - lineoff);
#	endif
	return (nextoff - lineoff);
}
