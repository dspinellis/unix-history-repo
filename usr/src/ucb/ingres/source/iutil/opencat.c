# include	<ingres.h>
# include	<aux.h>
# include	<opsys.h>
# include	<access.h>
# include	<sccs.h>

SCCSID(@(#)opencat.c	7.1	2/5/81)

/*
**  OPENCATALOG -- open system catalog
**
**	This routine opens a system catalog into a predetermined
**	cache.  If the catalog is already open, it is not reopened.
**
**	The 'Desxx' struct defines which relations may be opened
**	in this manner and is defined in .../source/aux.h.
**
**	The relation should not be closed after use (except in
**	special cases); however, it should be noclose'd after use
**	if the number of tuples in the catalog may have changed.
**
**	The Desxx structure has an alias field which
**	is the address of the 'Admin' structure cache which holds
**	the relation descriptor.  Thus, an openr need never actually
**	occur.
**
**	The actual desxx structure definition is in the file
**	
**		catalog_desc.c
**
**	which defines which relations can be cached and if any
**	alias descriptors exist for the relations. That file
**	can be redefined to include various caching.
**
**
**	Parameters:
**		name -- the name of the relation to open.  It must
**			match one of the names in the Desxx
**			structure.
**		mode -- just like 'mode' to openr.  If zero, it
**			is opened read-only; if two, it is opened
**			read/write.  In fact, the catalog is always
**			opened read/write, but the flags are set
**			right for concurrency to think that you are
**			using it as you have declared.
**
**	Returns:
**		none
**
**	Side Effects:
**		A relation is (may be) opened.
**
**	Trace Flags:
**		none
*/

opencatalog(name, mode)
char	*name;
int	mode;
{
	int			i;
	register DESC		*d;
	register char		*n;
	register struct desxx	*p;
	extern struct desxx	Desxx[];
	extern long		CmOfiles;

	n = name;

	/* find out which descriptor it is */
	for (p = Desxx; p->cach_relname; p++)
		if (sequal(n, p->cach_relname))
			break;
	if (!p->cach_relname)
		syserr("opencatalog: (%s)", n);

	d = p->cach_desc;

	/* if it's already open, just return */
	if (d->relopn)
	{
		clearkeys(d);
	}
	else
	{
		/* not open, open it */
		if (p->cach_alias)
		{
			acc_init();
			bmove((char *) p->cach_alias, (char *) d, sizeof (*d));
		}
		else
		{
			if ((i = openr(d, 2, n)) != 0)
				syserr("opencatalog: openr(%s) %d", n, i);
		}

		/* mark it as an open file */
		CmOfiles |= 1 << d->relfp;
	}

	/* determine the mode to mark it as for concurrency */
	switch (mode)
	{
	  case 0:	/* read only */
		d->relopn = abs(d->relopn);
		break;

	  case 2:	/* read-write */
		d->relopn = -abs(d->relopn);
		break;

	  default:
		syserr("opencatalog(%s): mode %d", n, mode);
	}

	return;
}
