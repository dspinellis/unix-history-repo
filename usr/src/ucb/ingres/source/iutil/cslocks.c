# include	<ingres.h>
# include	<access.h>
# include	<lock.h>
# include	<sccs.h>

SCCSID(@(#)cslocks.c	7.1	2/5/81)

struct lockreq	Lock;
/*
 *	setcsl- set a critical section lock
 */
setcsl(rtid)
long	rtid;
{
	register char	*r;
	register int	i;

#	ifdef xATR1
	if (tTf(28, 0))
	{
		printf(" setcsl ");
		dumptid(&rtid);
	}
#	endif

	if (Alockdes < 0)
		return (1);
	Lock.lract = A_SLP;	/* sleep while waiting on lock */
	Lock.lrtype = T_CS;	/* critical section lock */
	Lock.lrmod = M_EXCL;	/* exclusive access */
	bmove(&rtid, Lock.lrel, 4);	/* copy relid */ 
	r = Lock.lpage;
	for (i = 0; i < 4; i++)
			/* zero out pageid */
		*r++ = 0;
	i = write(Alockdes, &Lock, KEYSIZE+3);
	return (i);
}



/*
 *	unlcs- unlock a critical section
 */
unlcs(rtid)
long	rtid;
{
	register char	*r;
	register int	i;

#	ifdef xATR1
	if (tTf(28, 1))
	{
		printf(" unlcs ");
		dumptid(rtid);
	}
#	endif

	if (Alockdes < 0)
		return (1);
	Lock.lract = A_RLS1;
	Lock.lrtype = T_CS;
	bmove(&rtid, Lock.lrel, 4);	/* copy relation identifier */
	r = Lock.lpage;
	for (i = 0; i < 4; i++)
			/* zero out page id */
		*r++ = 0;
	i = write(Alockdes, &Lock, KEYSIZE + 3);
	return (i);
}
