# include	<ingres.h>
# include	<aux.h>
# include	<symbol.h>
# include	<access.h>
# include	<lock.h>
# include	<sccs.h>

SCCSID(@(#)ndxsearch.c	7.2	2/17/81)


/*
**  NDXSEARCH -- search an ISAM directory
**	
**	Looks for an available page, if this would force out a page from
**	the relation it adds its own.  Searches each level of the dirctory
**	for the lowest (highest) page on which the key could occur if mode
**	is < (>) 0.  At each level the number of keys on the page is checked
**	if it is <= SEARCHFUDGE a linear sharch is done, otherwize a binary
**	search is preformed.  If the full key matches exactly the seach
**	can stop.  Note that the keys tell the minimum value on that page.
**
**	Parameters:
**		d	- pointer to descriptor of open relation
**		tid	- returns tid of page to start (end) looking
**		key	- to look for
**		mode	- < 0 lowkey, > 0 hikey
**		keyok	- true if all keys are assumed to be set
**
**	Returns:
**		-2	- pageflush failure
**		-1	- get_page failure
**		0	- success
**
**	Side Effects:
**		causes I/O
*/

# define	SEARCHFUDGE	6	/* # of linenos to do a binary search */

ndxsearch(d, tid, tuple, mode, keyok)
DESC		*d;		
register TID	*tid;		
char		tuple[];	
int		mode;	
int		keyok;
{
	register int		i;
	long			pageid;
	int			j, keylen, dno, partialkey;
	char			*p;
	int			pagerr;
	struct accessparam	relparm;
	int			top;
	register		bottom;
	extern char		*get_addr();

	pagerr = 0;
	paramd(d, &relparm);	/* get domains in key order */

	/* assume fullkey is given */
	partialkey = FALSE;

	/* remove any key domains not given by the user */
	if (!keyok)
	{
		for (i = 0; dno = relparm.keydno[i]; i++)
		{
			if (d->relgiven[dno])
				continue;
			relparm.keydno[i] = 0;
			partialkey = TRUE;
			break;
		}
	}

	/* if the first key isn't given, just return else search directory */
	if (relparm.keydno[0] != 0)
	{
		/* The actual ISAM directory search must be performed */
		pageid = d->reldum.relprim;
		stuff_page(tid, &pageid);

		Acclock = FALSE;

		do
		{
			if (pagerr = get_page(d, tid))
				break;	/* fatal error */
			Acc_head->bufstatus |= BUF_DIRECT;  /* don't get confused */
			bottom = 0;
			top = Acc_head->nxtlino - 1;

			if (top >= SEARCHFUDGE)  /* we are past breakeven */
			{
				while (top != bottom)	/* binary search */
				{
					tid->line_id = ((1 + top - bottom) >> 1) + bottom;	/* get to half way point */
					p = get_addr(tid);
					for (j = 0; dno = relparm.keydno[j]; j++)
					{
						keylen = d->relfrml[dno] & I1MASK;
						if (i = icompare(&tuple[d->reloff[dno]], p, d->relfrmt[dno], keylen))
							break;
						p += keylen;
					}
					/* if key is below directory, then we are in the bottom half */
					if (i < 0 || (i == 0 && partialkey && mode < 0))
						top = (tid->line_id & I1MASK) - 1;

					else if (i == 0 && !partialkey)
					{
						bottom = (tid->line_id & I1MASK);
						break;
					}
					else
						bottom = (tid->line_id & I1MASK);
				}
			}
			else	/* do a linar search */
			{
				for (tid->line_id = 0; (tid->line_id & I1MASK) < Acc_head->nxtlino; tid->line_id++)
				{
					p = get_addr(tid);
					for (i = 0; dno = relparm.keydno[i]; i++)
					{
						keylen = d->relfrml[dno] & I1MASK;
						if (j = icompare(&tuple[d->reloff[dno]], p, d->relfrmt[dno], keylen))
							break;
						p += keylen;
					}
					/* if key is below directory, then we have passed the position */
					if (j < 0)
						break;

					/* if lowkey search on partial key matches, then done */
					if (j == 0 && partialkey && mode < 0)
						break;
					bottom = tid->line_id;
					if (j == 0 && mode < 0)
						break;
				}
			}
			pageid = Acc_head->ovflopg + bottom;
			stuff_page(tid, &pageid);
		} while (Acc_head->mainpg);  /* if at level zero then done */
		Acclock = TRUE;
	
	}
	tid->line_id = -1;
	return (pagerr);
}
