# include	<ingres.h>
# include	<lock.h>
# include	<sccs.h>

SCCSID(@(#)dblocks.c	7.1	2/5/81)

struct	lockreq	Lock;
/*
 *	setdbl - set data base lock for either exclusive or shared
 *		 access.
 */
setdbl(act, mod)
char	act;			/* type request */
char	mod;			/* mod of lock: = 1 for exclusive, = 2 for shared*/
{
	register char	*r;
	register int	i;

#	ifdef xATR1
	if (tTf(28, 6))
		printf(" setdbl act=%o md=%o\n", act, mod);
#	endif
	if (Alockdes < 0)
		return (1);
	Lock.lract = act;		/* type of request */
	Lock.lrtype = T_DB;		/* data base lock */
	Lock.lrmod = mod;		/* exclusive or shared */
					/* zero out rest of key */
	r = Lock.lrel;
	for (i = 0; i < 8; i++)
		*r++ = 0;
	i = write(Alockdes, &Lock, KEYSIZE + 3);
	return (i);
}
/*
 *	unldb	- release the data base lock
 */
unldb()
{
	register char	*r;
	register int	i;

#	ifdef xATR1
	if (tTf(28, 7))
		printf(" unldb\n");
#	endif
	if (Alockdes < 0)
		return (1);
	Lock.lract = A_RLS1;		/* release 1 lock */
	Lock.lrtype = T_DB;		/* a data base lock*/
	r = Lock.lrel;
	for (i = 0; i < 8; i++)		/* zero out part of key*/
		*r++ = 0;
	i = write(Alockdes, &Lock, KEYSIZE + 3);
	return (i);
}
