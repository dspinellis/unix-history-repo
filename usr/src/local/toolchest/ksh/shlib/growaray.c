/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/* @(#)growaray.c	1.1 */

/*
 *   GROWARAY.C
 *
 *   Programmer:  D. G. Korn
 *
 *        Owner:  D. A. Lambeth
 *
 *         Date:  April 17, 1980
 *
 *
 *
 *   GROWARAY (ARP, MAXI)
 *
 *        Create or expand the size of an array of Namnods, ARP,
 *        such that MAXI is a legal index into ARP.
 *
 *   SETDOT (NODE, INDEX)
 *
 *        Set the current index of the array NODE to be INDEX.
 *        (library only)
 *
 *
 *
 *   See Also:  linknod(III)
 */

#include	"name.h"
#include        "flags.h"

struct Namaray *growaray();
int	arsize ();
#ifndef KSHELL
void	setdot ();
#endif	/* KSHELL */

#define round(a,b)	((a+b-1)&~(b-1))
extern char	*malloc();
extern char	*itos();
extern void	failed();
extern void	free();

/*
 *   GROWARAY (ARP, MAXI)
 *
 *        struct Namaray *ARP;
 *
 *        int MAXI;
 *
 *        Increase the size of the array of Namnods given by ARP
 *        so that MAXI is a legal index.  If ARP is NULL, an array
 *        of the required size is allocated.  A pointer to the 
 *        allocated Namaray structure is returned.
 *
 *        MAXI becomes the current index of the array.
 */

struct Namaray *growaray(arp,maxi)
struct Namaray *arp;
{
	register struct Namaray *ap,*aq;
	register int cursize, i;
	register int newsize = arsize (maxi);
	cursize = ((arp == NULL) ? 0 : arsize ((int)arp->maxi));
	if (maxi >= ARRMAX)
		failed (itos(maxi), subscript);
	if (((aq = ap = arp) == NULL) || (newsize > cursize))
	{
		ap = (struct Namaray *)malloc((unsigned)(sizeof(struct Namaray)
				+ (newsize-1)*sizeof(struct Nodval*)));
		ap->maxi = maxi;
		for(i=0;i < newsize;i++)
			ap->val[i] = NULL;
		if(aq)
		{
			for(i=0;i <= aq->maxi;i++)
				ap->val[i] = aq->val[i];
			free((char *)aq);
		}
	}
	else
        	if (maxi > ap->maxi)
			ap->maxi = maxi;
	ap->adot = maxi;
	return(ap);
}

#ifndef KSHELL

/*
 *   SETDOT (NODE, INDEX)
 *
 *        struct Namnod *NODE;
 *
 *        int INDEX;
 *
 *   Given an array of Namnods NODE, set the current index of NODE
 *   to INDEX.  Trap if INDEX is out of bounds.  Otherwise allocate
 *   a Nodval for the INDEXth element of NODE, if necessary.
 */

void	setdot (node, index)
struct Namnod *node;
int index;
{
	register struct Nodval *nv;
	register struct Namaray *ap = arayp (node);

	if ((index > ap->maxi) || (index < 0))
		failed (node->namid, subscript);
	else
		ap->adot = index;
	if (ap->val[index] == NULL)
	{
		nv = (struct Nodval*)malloc (sizeof (struct Nodval));
		nv->namflg = node->value.namflg & ~ARRAY;
		nv->namval.cp = NULL;
		ap->val[index] = nv;
	}
	return;
}
#endif	/* KSHELL */

/*
 *   ARSIZE (MAXI)
 *
 *        int MAXI;
 *
 *   Calculate the amount of space to be allocated to hold
 *   an array into which MAXI is a legal index.  The number of
 *   elements that will actually fit into the array (> MAXI
 *   but <= ARRMAX) is returned.
 *
 *   ALGORITHM:  The size of an array can be incremented in 
 *               lots of ARRINCR elements.  Internal size is thus
 *               the least multiple of ARRINCR that is greater than
 *               MAXI.  (Note that 0-origin indexing is used.)
 */

int	arsize (maxi)
register int maxi;
{
	register int i = round(maxi+1,ARRINCR);
	return (i>ARRMAX?ARRMAX:i);
}
