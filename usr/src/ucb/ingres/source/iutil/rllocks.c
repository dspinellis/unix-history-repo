# include	<ingres.h>
# include	<lock.h>
# include	<sccs.h>

SCCSID(@(#)rllocks.c	7.1	2/5/81)

struct lockreq	Lock;
/*
/*
 *	setrll- set a relation lock
 */
setrll(act, rtid, mod)
char	act;
long	rtid;
char	mod;
{
	register char	*r;
	register int	i;

#	ifdef xATR1
	if (tTf(28, 4))
	{
		printf(" setrll act=%d md=%o ", act, mod);
		dumptid(&rtid);
	}
#	endif
	if (Alockdes < 0)
		return(1);
	Lock.lract = act;	/* sleep (act = 2) or error return (act = 1)*/
	Lock.lrtype = T_REL;	/* relation lock */
	Lock.lrmod = mod;	/* exclusive (mod = 1) or shared (mod = 2)*/
	bmove(&rtid, Lock.lrel, 4);	/* copy relation id */
	r = Lock.lpage;
	for (i = 0; i < 4; i++)
			/* zero out page id */
		*r++ = 0;
	i = write(Alockdes, &Lock, KEYSIZE+3);
	return(i);
}
/*
 *	unlrel- unlock a relation lock
 */
unlrl(rtid)
long	rtid;
{
	register char	*r;
	register int	i;

#	ifdef xATR1
	if (tTf(28, 5))
	{
		printf(" unlrl ");
		dumptid(&rtid);
	}
#	endif
	if (Alockdes < 0)
		return (1);
	Lock.lract = A_RLS1;
	Lock.lrtype = T_REL;	/* relation lock */
	bmove(&rtid, Lock.lrel, 4);	/* copy relation id */
	r = Lock.lpage;
	for (i = 0; i < 4; i++)
			/* zero out pageid */
		*r++ = 0;
	i = write(Alockdes, &Lock, KEYSIZE + 3);
	return (i);
}
