# include	<ingres.h>
# include	<catalog.h>
# include	<sccs.h>

SCCSID(@(#)get_p_tid.c	7.1	2/5/81)

/*
**  GET_P_TID -- Get the primary tid for a relation for locking
**
**	Finds the correct tid for locking the relation. If the
**	relation is a primary relation, then the tid of the
**	relation is returned.
**
**	If the relation is a secondary index then the tid of the
**	primary relation is returned.
**
**	Parameters:
**		des - an open descriptor for the relation.
**		tidp - a pointer to a place to store the tid.
**
**	Returns:
**		none
**
**	Side Effects:
**		alters the value stored in "tidp",
**		may cause access to the indexes relation
**
**	Called By:
**		modify
*/



get_p_tid(d, tp)
register DESC	*d;
register TID	*tp;
{
	register int	i;
	struct index	indkey, itup;
	DESC		ides;
	extern DESC	Inddes;

	if (d->reldum.relindxd < 0)
	{
		/* this is a secondary index. lock the primary rel */
		opencatalog("indexes", 0);
		setkey(&Inddes, &indkey, d->reldum.relowner, IOWNERP);
		setkey(&Inddes, &indkey, d->reldum.relid, IRELIDI);
		if (getequal(&Inddes, &indkey, &itup, tp))
			syserr("No prim for %.14s", d->reldum.relid);

		if (i = openr(&ides, -1, itup.irelidp))
			syserr("openr prim %d,%.14s", i, itup.irelidp);

		bmove(&ides.reltid, tp, sizeof (*tp));
	}
	else
		bmove(&d->reltid, tp, sizeof (*tp));
}
