# include	<ingres.h>
# include	<aux.h>
# include	<access.h>
# include	<sccs.h>

SCCSID(@(#)findbest.c	7.1	2/5/81)


/*
**	Findbest - find the "best" place to put a tuple.
**	Findbest does not actually put the tuple but rather
**	returns and allocates the tid for the tuple.
**
**	The initial part of the algorithm depends on whether
**	the relation is a heap or not.
**
**	If the relation is a heap, if there is a current page
**	with room for the tuple, that page is used. Otherwise
**	the last page of the heap is considered.
**
**	If the relation is hash or isam, then "find" is used
**	to determine the primary page for the tuple.
**
**	If necessary, findbest will allocate an overflow page
**	if there is not sufficient room for the tuple otherwise.
**
**	If checkdups is TRUE and the relation is not a heap,
**	findbest will check for duplicates.
**
**	Returns:
**
**		0 tuple not a duplicate, tid allocated
**		1 tuple a duplicate of the tuple at tid
*/

findbest(dx, tidx, tuple, need, checkdups)
DESC	*dx;
TID	*tidx;
char	*tuple;
int	need;
bool	checkdups;
{
	register DESC	*d;
	register TID	*tid;
	register int	i;
	TID		temptid;

	d = dx;
	tid = tidx;


	if (abs(d->reldum.relspec) == M_HEAP)
	{
		checkdups = FALSE;
		/* determine a page to place tuple in heap relation */
		find_page(d, tid, need);

	}
	else
	{
		/* find a suitable page for isam or hash */
		/* determine primary page */
		if (i = find(d, FULLKEY, tid, tid, tuple))
		{
			return (i);	/* fatal error */
		}

		/* If we are not checking for duplicates then take any
		** convenient page linked to the main page current indicated
		** in "tid"
		*/
		if (!checkdups)
			find_page(d, tid, need);
	}

	/* search the chain of pages looking for a spot */
	for (;;)
	{
		if (i = get_page(d, tid))
			break;		/* fatal error */

		/* if tuple is duplicate, drop out */
		if (checkdups && dup_check(d, tid, tuple))
		{
			i = 1;
			break;
		}

		/* is there space on this page */
		if (space_left(Acc_head) >= need)
			break;	/* found a page to use */

		/* no space yet. look on next overflow page */
		if (Acc_head->ovflopg)
		{
			stuff_page(tid, &Acc_head->ovflopg);
			continue;
		}

		/* no space. allocate new overflow page */
		if (i = add_ovflo(d, tid))
			break;		/* fatal error */
	}

	/* check for dups on remaining overflow pages */
	/* check only if there hasn't been a dup or a page error */
	if (i == 0 && checkdups && Acc_head->ovflopg)
	{
		stuff_page(&temptid, &Acc_head->ovflopg);
		if (i = scan_dups(d, &temptid, tuple))
			bmove((char *) &temptid, (char *) tid, sizeof(temptid));	/* tid of duplicate */
	}

	/* if tuple isn't a duplicate, allocate a line number */
	if (i == 0)
		tid->line_id = newlino(need);

#	ifdef xATR1
	if (tTf(27, 0))
	{
		printf("findbest ret %d,", i);
		dumptid(tid);
	}
#	endif
	return (i);
}
/*
**	FINDBEST -- find best page for tuple
**
**	Find an appropriate page to put a tuple.
**	If HEAP then any page with room will do. If none
**	can be found, then use the last page.
**	If it is a user relation and a page was found but
**	was full, use it anyway. This can happen only on a
**	modify (which has checkdups turned off).
**
**	For ISAM or HASH look for a page on the same mainpage
**	chain. Duplicate checking must not be enforced.
**
**	The first page to use will be returned in tid in either
**	case.
*/

find_page(dx, tid, need)
DESC	*dx;
TID	*tid;
int	need;
{
	register DESC		*d;
	register struct accbuf	*b, *maxbf;
	int			heap;
	long			mainpg;

	d = dx;
	maxbf = NULL;
	heap = abs(d->reldum.relspec) == M_HEAP;
	pluck_page(tid, &mainpg);
	mainpg++; /* mainpage in buffer points to next higher mainpage */

	/* scan all current buffers looking for one belonging to this relation */
	for (b = Acc_head; b != NULL; b = b->modf)
	{
		if (d->reltid.ltid == b->rel_tupid && !(b->bufstatus & BUF_DIRECT)
			&& (heap || (b->mainpg == mainpg)))
		{
			if (space_left(b) >= need)
			{
				/* use this page */
				stuff_page(tid, &b->thispage);
				return;
			}

			/* save buffer of largest page */
			if (maxbf == NULL || maxbf->thispage < b->thispage)
				maxbf = b;
		}
	}

	if (heap)
		last_page(d, tid, maxbf);
	else
	{
		/* if we found a full page of a user's relation,use it */
		if (maxbf && (d->reldum.relstat & S_CATALOG) == 0)
			stuff_page(tid, &maxbf->thispage);
	}
}
