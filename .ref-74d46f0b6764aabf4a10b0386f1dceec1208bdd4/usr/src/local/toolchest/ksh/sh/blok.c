/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/* @(#)blok.c	1.1 */
/*
 *	UNIX shell
 *
 *	S. R. Bourne
 *	Rewritten by David Korn
 *	AT&T Bell Laboratories
 *
 */

#include	"defs.h"
#include	"stak.h"
#include	"brkincr.h"


/*
 *	storage allocator
 *	(circular first fit strategy)
 */

#define BUSY 01
#define busy(x)	(Rcheat((x)->word)&BUSY)

void	addblok();
void	free();
char	*malloc();
void	setbrk();
#ifdef DBUG
void	chkmem();
extern	void	p_str();
extern	void	p_num();
extern	void	p_flush();
#endif	/* DBUG */


/*
 * equivalent to malloc(3) except that a data area stack is
 * maintained on top of the heap
 */

char	*malloc(nbytes)
unsigned 	nbytes;
{
	register unsigned  rbytes = round(nbytes+BYTESPERWORD,BYTESPERWORD);
	while(1)
	{
		register BLKPTR p = blokp;
		register BLKPTR q;
		register int c=0;
		do
		{
			 if(!busy(p))
			{
				while(!busy(q = p->word))
					p->word = q->word;
				if(ADR(q)-ADR(p) >= rbytes)
				{
					blokp = BLK(ADR(p)+rbytes);
					if(q > blokp)
						blokp->word = p->word;
					p->word=BLK(Rcheat(blokp)|BUSY);
					return(ADR(p+1));
				}
			}
			q = p; p = BLK(Rcheat(p->word)&~BUSY);
		}
		while(p>q || (c++)==0);
		addblok(rbytes);
	}
}

/*
 * add more space to the heap and move the stack to the top of the heap
 */

void	addblok(reqd)
register unsigned int reqd;
{
	if(stakbot == 0)
	{
		setbrk(3*BRKINCR);
		bloktop = BLK(brkbegin);
	}
	if(stakbas!=staktop)
	{
		register STKPTR	rndstak;
		register BLKPTR	blokstak;
		pushstak(0);
		rndstak=(STKPTR) round(staktop,BYTESPERWORD);
		blokstak=BLK(stakbas)-1;
		blokstak->word=stakbsy; stakbsy=blokstak;
		bloktop->word=BLK(Rcheat(rndstak)|BUSY);
		bloktop=BLK(rndstak);
	}
	reqd += BRKINCR;
	reqd &= ~(BRKINCR-1);
	blokp=bloktop;
	bloktop=bloktop->word=BLK(Rcheat(bloktop)+reqd);
	reqd = 0;
	while((char*)bloktop+BRKINCR > brkend+reqd)
		reqd += BRKINCR;
	if(reqd)
		setbrk((int)reqd);
	bloktop->word=BLK(Rcheat(brkbegin)|BUSY);
	{
		register STKPTR stakadr=STK(bloktop+2);
		register STKPTR sp = stakadr;
		if(reqd = (staktop-stakbot))
		{
			while(reqd-- > 0)
				*sp++ = *stakbot++;
			sp--;
		}
		staktop = sp;
		stakbas=stakbot=stakadr;
	}
}

/*
 * mark the block free if address is in the heap
 */

void	free(ap)
register char	*ap;
{
	register BLKPTR p;
	if(ap>brkbegin && ap<(char*)bloktop)
	{
		p = (BLKPTR)(ap-sizeof(p->word));
		p->word = (BLKPTR)(Rcheat(p->word)&~BUSY);
	}
}


void setbrk(incr)
{
	register char *a=(char *)(sbrk(incr));
	if((int)a == -1)
		error(nospace);
	a = (char*)round(a,BYTESPERWORD);
	if(brkbegin==0)
		brkbegin = a;
	brkend=a+incr-8;
#ifndef INT16
	if(brkend > brkbegin + BRKMAX)
	{
		error(nospace);
	}
#endif	/* INT16 */
}

#ifdef DBUG
void chkmem()
{
	register BLKPTR	p = (BLKPTR)brkbegin;
	register BLKPTR	q;
	register int 	us=0, un=0;

	while(1)
	{
		q = (BLKPTR) (Rcheat(p->word)&~BUSY);

		if(q<BLK(brkbegin) || q>bloktop)
		abort(3);
		if(p==bloktop)
			break;
		if(busy(p))
			us += q-p;
		else
		  	 un += q-p;
		if(p>=q)
		{
			p_flush();
			abort(4);
		}
		 p=q;
	}
	un *= sizeof(*q);
	us *= sizeof(*q);
	p_str("free/used/missing",':');
	p_num(un,' ');
	p_num(us,' ');
	p_num((char*)bloktop - (char*)brkbegin - (un+us),NL);
}

/*
 * returns 1 if <ap> is on heap and is free
 * returns 2 if <ap> is on heap and not beginning of a block
 * otherwise returns 0
 */

int	chkfree(ap)
register char	*ap;
{
	register BLKPTR p;
	if(ap>brkbegin && ap<(char*)bloktop)
	{
		p = (BLKPTR)(ap-sizeof(p->word));
		if(p->word<BLK(brkbegin) || p->word>bloktop)
			return(2);
		return(!busy(p));
	}
	return(0);
}
#endif	/* DBUG */
