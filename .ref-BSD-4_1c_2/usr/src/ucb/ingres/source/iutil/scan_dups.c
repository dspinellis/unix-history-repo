# include	<ingres.h>
# include	<symbol.h>
# include	<access.h>
# include	<lock.h>
# include	<sccs.h>

SCCSID(@(#)scan_dups.c	7.1	2/5/81)


/*
**	Scan for duplicates. Start with the page
**	specified by tid and
**	continue until there are no more pages
**
**	Scan_dups guarantees that the same page will
**	be in the buffer on entry and exit
**
**	returns:
**		<0 if fatal error
**		0  if not duplicate
**		1  if duplicate
*/

scan_dups(d, tid, tuple)
DESC		*d;
register TID	*tid;
char		*tuple;
{
	register int	dup, pagerr;
	TID		savetid;

	stuff_page(&savetid, &Acc_head->thispage);

	Acclock = FALSE;	/* turn concurrency off */
	dup = 0;	/* assume success */

	do
	{
		if (pagerr = get_page(d, tid))
			break;	/* fatal error */

		if (dup = dup_check(d, tid, tuple))
			break;	/* found duplicate */

		stuff_page(tid, &Acc_head->ovflopg);
	} while (Acc_head->ovflopg);

	if (pagerr || (pagerr = get_page(d, &savetid)))
		dup = pagerr;	/* return fatal page error */

	Acclock = TRUE;		/* turn concurency on */
	return (dup);
}
/*
**  DUP_CHECK -- check current page for a duplicate of tuple;
**
**	returns:
**		0 if not duplicate
**		1 if duplicate
*/

dup_check(d, tid, tuple1)
register DESC	*d;
TID		*tid;
char		*tuple1;
{
	register int	i, dom;
	short		*lp;
	int		len, lastline, tups_equal;
	char		*tup1, *tup2;
	char		tuple2[MAXTUP];
	char		*getint_tuple();

	/* determine starting and ending points */
	lastline = Acc_head->nxtlino;
	lp = Acc_head->linetab;

	/* check each tuple for duplication */
	for (i = 0; i < lastline; i++)
	{
		/* is this line used? */
		if (*lp--)
		{
			/* yes. get tuple and check it */
			tid->line_id = i;
			tup2 = getint_tuple(d, tid, tuple2);
			tup1 = tuple1;
			tups_equal = TRUE;	/* assume equal */

			/* now check each domain for duplication */
			for (dom = 1; dom <= d->reldum.relatts; dom++)
			{
				len = d->relfrml[dom] & I1MASK;
				if (d->relfrmt[dom] == CHAR)
					tups_equal = scompare(tup1, len, tup2, len) == 0;
				else
					tups_equal = bequal(tup1, tup2, len);
				if (!tups_equal)
				{
					/* not duplicates */
					break;
				}
				tup1 += len;
				tup2 += len;
			}
			if (tups_equal)
				return (1);	/* duplicate */
		}
	}
	return (0);	/* no duplicate */
}
