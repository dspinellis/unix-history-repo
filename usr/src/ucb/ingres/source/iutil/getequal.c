# include	<ingres.h>
# include	<access.h>
# include	<sccs.h>

SCCSID(@(#)getequal.c	7.1	2/5/81)


/*
**	getequal - get the first tuple equal to the provided key
**	
**	GETEQUAL is used to do a keyed retrieval of a single
**	tuple in cases where the calling program knows the key to
**	be unique.  SETKEY must be called first to set all desired
**	domain values.  GETEQUAL will return the first tuple with
**	equality on all of the specified domains.
**	The tuple is returned in TUPLE.
**	
**	function values:
**	
**		<0 fatal error
**		 0  success
**		 1  no match
**
**	Trace Flags:
**		23.8-15
*/


getequal(d, key, tuple, tid)
register DESC	*d;
char		key[MAXTUP];
char		tuple[MAXTUP];
TID		*tid;
{
	TID		limtid;
	register int	i;

#	ifdef xATR1
	if (tTf(23, 8))
	{
		printf("getequal: %.14s,", d->reldum.relid);
		printdesc(d);
		printup(d, key);
	}
#	endif
	if (i = find(d, EXACTKEY, tid, &limtid, key))
		return (i);
	while ((i = get(d, tid, &limtid, tuple, TRUE)) == 0)
	{
		if (!kcompare(d, key, tuple))
		{
#			ifdef xATR2
			if (tTf(23, 9))
			{
				printf("getequal: ");
				dumptid(tid);
				printf("getequal: ");
				printup(d, tuple);
			}
#			endif
			return (0);
		}
	}
#	ifdef xATR2
	if (tTf(23, 10))
		printf("getequal: %d\n", i);
#	endif
	return (i);
}
