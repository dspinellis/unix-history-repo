# include	<ingres.h>
# include	<access.h>
# include	<sccs.h>

SCCSID(@(#)insert.c	7.1	2/5/81)

/*
**	INSERT - add a new tuple to a relation
**
**	Insert puts a given tuple into a relation in
**	the "correct" position.
**
**	If insert is called with checkdups == TRUE then
**	the tuple will not be inserted if it is a duplicate
**	of some already existing tuple. If the relation is a
**	heap then checkdups is made false.
**
**	Tid will be set to the tuple id where the
**	tuple is placed.
**
**	returns:
**		<0  fatal eror
**		0   success
**		1   tuple was a duplicate
*/

insert(d, tid, tuple, checkdups)
register DESC	*d;
register TID	*tid;
char		*tuple;
bool		checkdups;
{
	register int	i;
	int		need;

#	ifdef xATR1
	if (tTf(24, 0))
	{
		printf("insert:%.14s,", d->reldum.relid);
		dumptid(tid);
		printup(d, tuple);
	}
#	endif
	/* determine how much space is needed for tuple */
	need = canonical(d, tuple);

	/* find the "best" page to place tuple */
	if (i = findbest(d, tid, tuple, need, checkdups))
		return (i);

	/* put tuple in position "tid" */
	put_tuple(tid, Acctuple, need);

	d->reladds++;

	return (0);
}
