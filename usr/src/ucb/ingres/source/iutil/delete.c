# include	<ingres.h>
# include	<access.h>
# include	<sccs.h>

SCCSID(@(#)delete.c	7.1	2/5/81)

/*
**	Delete - delete the tuple specified by tid
**
**	Delete removes the tuple specified by tid
**	and reclaims the tuple space.
**
**	returns:
**		<0  fatal error
**		0   success
**		2   tuple specified by tid aleady deleted
*/

delete(dx, tidx)
DESC	*dx;
TID	*tidx;
{
	register DESC	*d;
	register TID	*tid;
	register int	i;

	d = dx;
	tid = tidx;

#	ifdef xATR1
	if (tTf(24, 8))
	{
		printf("delete: %.14s,", d->reldum.relid);
		dumptid(tid);
	}
#	endif

	if (i = get_page(d, tid))
		return (i);

	if (i = invalid(tid))
		return (i);

	i = tup_len(tid);

	del_tuple(tid, i);
	d->reladds--;

	return (0);
}
