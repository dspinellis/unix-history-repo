# include	<ingres.h>
# include	<aux.h>
# include	<access.h>
# include	<sccs.h>

SCCSID(@(#)noclose.c	7.1	2/5/81)

/*
**	noclose - update system catalogs for a relation
**	DESCRIPTION
**
**	function values:
**
**		<0  fatal error
**		 0  success
**		 1  relation was not open
*/


noclose(d)
register DESC	*d;
{
	register int	i;
	struct relation	rel;

#	ifdef xATR1
	if (tTf(21, 12))
		printf("noclose: %.14s,%ld\n", d->reldum.relid, d->reladds);
#	endif

	/* make sure relation relation is read/write mode */
	if (abs(d->relopn) != (d->relfp + 1) * 5)
		return (1);

	/* flush all pages associated with relation */
	/* if system catalog, reset all the buffers so they can't be reused */
	i = flush_rel(d, d->reldum.relstat & S_CATALOG);

	/* check to see if number of tuples has changed */
	if (d->reladds != 0)
	{
		/* yes, update the system catalogs */
		/* get tuple from relation relation */
		Admin.adreld.relopn = (Admin.adreld.relfp + 1) * -5;
		if (i = get_page(&Admin.adreld, &d->reltid.s_tupid))
			return (i);	/* fatal error */

		/* get the actual tuple */
		get_tuple(&Admin.adreld, &d->reltid.s_tupid, (char *) &rel);

		/* update the reltups field */
		rel.reltups += d->reladds;
		d->reldum.reltups = rel.reltups;

		/* put the tuple back */
		put_tuple(&d->reltid.s_tupid, (char *) &rel, sizeof rel);
		i = resetacc(Acc_head);
		d->reladds = 0;
	}
	return (i);
}
