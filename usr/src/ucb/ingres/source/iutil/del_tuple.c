# include	<ingres.h>
# include	<access.h>
# include	<sccs.h>

SCCSID(@(#)del_tuple.c	7.1	2/5/81)

/*
**	Delete the specified tuple from the
**	current page.
**
**	The space occupied by the tuple is
**	compacted and the effected remaining
**	tuple line offsets are adjusted.
*/

del_tuple(tid, width)
TID	*tid;
int	width;
{
	register char	*startpt, *midpt;
	register int	i;
	extern char	*get_addr();
	char		*endpt;
	int		cnt, offset, nextline;
	int		linenum;

	linenum = tid->line_id & I1MASK;
	offset = Acc_head->linetab[-linenum];
	nextline = Acc_head->nxtlino;

	startpt = get_addr(tid);
	midpt = startpt + width;
	endpt = (char *)Acc_head + Acc_head->linetab[-nextline];

	cnt = endpt - midpt;

	/* delete tuple */
	Acc_head->linetab[-linenum] = 0;

	/* update affected line numbers */
	for (i = 0; i <= nextline; i++)
	{
		if (Acc_head->linetab[-i] > offset)
			Acc_head->linetab[-i] -= width;
	}

	/* compact the space */
	while (cnt--)
		*startpt++ = *midpt++;
	Acc_head->bufstatus |= BUF_DIRTY;
}
