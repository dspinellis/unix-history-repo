# include	<ingres.h>
# include	<access.h>
# include	<lock.h>
# include	<sccs.h>

SCCSID(@(#)pglocks.c	7.1	2/5/81)

struct lockreq	Lock;
/*
 *	setpgl- sets a lock for the access buffer
 */
setpgl(buf)
struct	accbuf	*buf;
{
	register struct accbuf	*b;
	register int		i;

#	ifdef xATR1
	if (tTf(28, 2))
	{
		printf(" setpgl pg=%ld rel", buf->thispage);
		dumptid(&buf->rel_tupid);
	}
#	endif
	if (Alockdes < 0)
		return(1);
	b = buf;
	Lock.lract = A_SLP;	/* wait for lock */
	Lock.lrtype = T_PAGE;	/* page lock */
	Lock.lrmod = M_EXCL;	/* exclusive lock */
	bmove(&b->rel_tupid, Lock.lrel, 4);	/* copy relation id */
	bmove(&b->thispage, Lock.lpage, 4);	/* copy page id */
	i = write(Alockdes,  &Lock,  KEYSIZE + 3);
	b->bufstatus |= BUF_LOCKED;
	return(i);
}
/*
 *	unlpg- releases a page lock
 */
unlpg(buf)
struct	accbuf	*buf;
{
	register struct	accbuf	*b;
	register int		i;

#	ifdef xATR1
	if (tTf(28, 3))
	{
		printf(" unlpg page %ld rel", buf->thispage);
		dumptid(&buf->rel_tupid);
	}
#	endif
	if (Alockdes < 0)
		return(1);
	b = buf;
	Lock.lract = A_RLS1;
	bmove(&b->rel_tupid, Lock.lrel, 4);	/* copy relation id */
	Lock.lrtype = T_PAGE;	/* page lock */
	bmove(&b->thispage, Lock.lpage, 4);	/* copy page id */
	b->bufstatus &= ~BUF_LOCKED;
	i = write(Alockdes,  &Lock,  KEYSIZE + 3);
	return(i);
}
/*
 *	unlall - release all locks held by this process
 */
unlall()
{
	register int	i;


#	ifdef xATR1
	if (tTf(28, 6))
		printf(" unlall\n");
#	endif

	Acclock = TRUE;	/* reset page lock flag just in case */
	if (Alockdes < 0)
		return(1);
	Lock.lract = A_RLSA;
	i = write(Alockdes, &Lock, KEYSIZE + 3);
	return (i);
}
