#ifndef lint
static char sccsid[] = "@(#)stak.c	4.2 8/11/83";
#endif

#
/*
 * UNIX shell
 *
 * S. R. Bourne
 * Bell Telephone Laboratories
 *
 */

#include	"defs.h"

STKPTR		stakbot=nullstr;



/* ========	storage allocation	======== */

STKPTR	getstak(asize)
	INT		asize;
{	/* allocate requested stack */
	REG STKPTR	oldstak;
	REG INT		size;

	size=round(asize,BYTESPERWORD);
	oldstak=stakbot;
	staktop = stakbot += size;
	return(oldstak);
}

STKPTR	locstak()
{	/* set up stack for local use
	 * should be followed by `endstak'
	 */
	IF brkend-stakbot<BRKINCR
	THEN	setbrk(brkincr);
		IF brkincr < BRKMAX
		THEN	brkincr += 256;
		FI
	FI
	return(stakbot);
}

STKPTR	savstak()
{
	assert(staktop==stakbot);
	return(stakbot);
}

STKPTR	endstak(argp)
	REG STRING	argp;
{	/* tidy up after `locstak' */
	REG STKPTR	oldstak;
	*argp++=0;
	oldstak=stakbot; stakbot=staktop=round(argp,BYTESPERWORD);
	return(oldstak);
}

VOID	tdystak(x)
	REG STKPTR 	x;
{
	/* try to bring stack back to x */
	WHILE ADR(stakbsy)>ADR(x)
	DO free(stakbsy);
	   stakbsy = stakbsy->word;
	OD
	staktop=stakbot=max(ADR(x),ADR(stakbas));
	rmtemp(x);
}

stakchk()
{
	IF (brkend-stakbas)>BRKINCR+BRKINCR
	THEN	setbrk(-BRKINCR);
	FI
}

STKPTR	cpystak(x)
	STKPTR		x;
{
	return(endstak(movstr(x,locstak())));
}
