# include	<ingres.h>
# include	<access.h>
# include	<sccs.h>

SCCSID(@(#)replace.c	7.1	2/5/81)

# define	SAMETUP		0
# define	SAMEKEYS	1
# define	DIFFTUP		2

/*
**  REPLACE - replace an already existing tuple
**
**	Replace will replace the tuple specified by TID
**	with the new tuple. An attempt is made to not
**	move the tuple if at all possible.
**
**	Three separate conditions are delt with. If the
**	new tuple is the same as the old tuple, a return 
**	of zero occures and the page is not changed.
**
**	If the keys(if any) are the same and the canonical
**	tuple lengths are the same, then the new tuple will
**	be placed in the same location.
**
**	If the lengths or the keys are different, then the
**	tuple is deleted and the new tuple inserted
**
**	Checkdups specifies whether to check for duplicates.
**	If the new tuple is a duplicate of one already there,
**	then the tuple at TID is deleted
**
**	Returns:
**		<0  fatal error
**		 1  new tuple was duplicate of returned tid
**		 2  tuple identified by tid has been deleted
**
**		If replace returns 1 then tid is set to the
**		duplicate tuple. This is necessary for updating
**		secondary indices.
**
**	Trace Flags:
**		24.4-7
*/


replace(d, tid, tuple, checkdups)	
register DESC	*d;
register TID	*tid;
char		*tuple;
int		checkdups;
{
	register int	i;
	char		oldtuple[MAXTUP];
	TID		primtid;
	long		primpage;
	int		need, same;
	int		len, oldlength;
	char		*new, *old, *oldt;
	char		*getint_tuple();

#	ifdef xATR1
	if (tTf(24, 4))
	{
		printf("replace: %.14s,", d->reldum.relid);
		dumptid(tid);
		printf("replace: ");
		printup(d, tuple);
	}
#	endif

	/* make tuple canonical */
	need = canonical(d, tuple);

	/* if heap, no dup checking */
	if (abs(d->reldum.relspec) == M_HEAP)
		checkdups = FALSE;

	if (i = get_page(d, tid))
		return (i);	/* fatal error */

	/* check if tid exists */
	if (i = invalid(tid))
		return (i);	/* already deleted or invalid */

	oldt = getint_tuple(d, tid, oldtuple);
	oldlength = tup_len(tid);

	/* check whether tuples are the same, different lengths, different keys */
	same = DIFFTUP;	/* assume diff lengths or keys */
	if (oldlength == need)
	{
		/* same size. check for same domains */
		same = SAMETUP;	/* assume identical */
		new = tuple;
		old = oldt;
		for (i = 1; i <= d->reldum.relatts; i++)
		{
			len = d->relfrml[i] & I1MASK;
			if (icompare(new, old, d->relfrmt[i], len))
			{
				if (d->relxtra[i])
				{
					same = DIFFTUP;
					break;
				}
				same = SAMEKEYS;
			}
			old += len;
			new += len;
		}
	}

#	ifdef xATR2
	if (tTf(24, 5))
		printf("replace:same=%d\n", same);
#	endif
	switch (same)
	{

	  case SAMETUP:
		/* new tuple same as old tuple */
		i = 1;	/* flag as duplicate */
		/* though character strings may compare equal,
		**  they can look different, so if they do look different
		**  go ahead and do the replace using put_tuple.  */
		if (!bequal(tuple, oldt, d->reldum.relwid))
			goto puttuple;
		break;

	  case SAMEKEYS:
		/* keys the same, lengths the same, tuples different */
		if (checkdups)
		{
			/* This is either an ISAM or HASH file. If mainpg
			** is non-zero, then the primary page=mainpg -1.
			** Otherwise, "find" must be called to determine
			** the primary page
			*/
			if (Acc_head->mainpg)
			{
				primpage = Acc_head->mainpg -1;
				stuff_page(&primtid, &primpage);
			}
			else
			{
				if (i = find(d, FULLKEY, &primtid, &primtid, tuple))
					return (i);	/* fatal error */
				if (i = get_page(d, tid))	/* restore page for tuple */
					return (i);
			}
	
			if (i = scan_dups(d, &primtid, tuple))
			{
				if (i == 1)
				{
					del_tuple(tid, oldlength);	/* tuple a duplicate */
					d->reladds--;
					/* copy tid of duplicate tuple */
					bmove(&primtid, tid, sizeof(primtid));
				}
				break;
			}
		}
		goto puttuple;

	  case DIFFTUP:
		/* keys different or lengths different */
		del_tuple(tid, oldlength);

		/* find where to put tuple */
		if (i = findbest(d, tid, tuple, need, checkdups))
		{
			d->reladds--;
			break;
		}

		/* place new tuple in page */
	puttuple:
		put_tuple(tid, Acctuple, need);
		i = 0;
	}

#	ifdef xATR1
	if (tTf(24, 6))
	{
		printf("replace rets %d,", i);
		dumptid(tid);
	}
#	endif

	return (i);
}
