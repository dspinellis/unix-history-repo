/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/* @(#)stak.c	1.1 */
/*
 * UNIX shell
 *
 * S. R. Bourne
 * Rewritten by David Korn
 * AT&T Bell Laboratories
 *
 */

#include	"defs.h"
#include	"stak.h"
#include	"brkincr.h"

extern char	*movstr();
extern void	setbrk();
extern void	rmtemp();
extern void	free();



/* ========	storage allocation	======== */

/*
 * allocate requested stack
 */

STKPTR	getstak(asize)
int 	asize;
{
	register STKPTR oldstak;
	register int 	size;
	size=round(asize,BYTESPERWORD);
	oldstak=stakbot;
	staktop = stakbot += size;
	if(staktop >= brkend)
		setbrk(round(staktop-brkend,BRKINCR));
	return(oldstak);
}

/*
 * set up stack for local use
 * should be followed by `endstak'
 */

STKPTR	locstak()
{
	if(brkend-stakbot<BRKINCR)
		setbrk(BRKINCR);
	return(staktop=stakbot);
}


/*
 * tidy up after `locstak'
 */

STKPTR	endstak(argp)
register char *argp;
{
	register STKPTR oldstak;
	*argp++=0;
	oldstak=stakbot; stakbot=staktop=(STKPTR) round(argp,BYTESPERWORD);
	return(oldstak);
}

/*
 * try to bring stack back to x
 */

void	tdystak(x)
register STKPTR  x;
{
	while(ADR(stakbsy)>ADR(x))
	{
		free((char*)stakbsy);
		stakbsy = stakbsy->word;
	}
	staktop=stakbot=max(ADR(x),ADR(stakbas));
	if(iotemp > (IOPTR)x)
		rmtemp((IOPTR)x);
}

stakchk()
{
	register int size = -3*BRKINCR;
#ifdef INT16
	if((brkend-stakbas) >= BRKMAX)
		size = BRKMAX;
	else
#endif /* INT16 */
		size += round(brkend-stakbas,BRKINCR);
	if(size > 0)
		setbrk(-size);
}

STKPTR	cpystak(x)
STKPTR		x;
{
	return(endstak(movstr(x,((char*)locstak()))));
}
