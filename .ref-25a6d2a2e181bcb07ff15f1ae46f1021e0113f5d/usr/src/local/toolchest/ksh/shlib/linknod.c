/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/* @(#)linknod.c	1.1 */

/*
 *   LINKNOD.C
 *
 *   Programmer:  D. G. Korn
 *
 *        Owner:  D. A. Lambeth
 *
 *         Date:  April 17, 1980
 *
 *
 *
 *   LINKNOD (NODP, ROOT)
 *        
 *        Link the node given by NODP into the memory tree
 *        given by ROOT.
 *
 *   RMNVAL (NV)
 *
 *        Remove freeable space associated with the Nodval NV.
 * 
 *
 *
 *   See Also:  findnod(III), gettree(III)
 */

#include	"name.h"
#include	<stdio.h>
#include        "flags.h"

void	linknod();
void	rmnval ();

extern struct Namaray *growaray();
extern struct Namnod *mak_nod();
extern unsigned	chkid();
extern void	free();

/*
 *   LINKNOD (NODP, ROOT)
 *
 *        struct Namnod *NODP;
 *
 *        struct Amemory *ROOT;
 *
 *   Link the Namnod pointed to by NODP into the memory tree
 *   denoted by ROOT.  If ROOT contains another Namnod with
 *   the same namid as NODP, an array is created, with the
 *   previously inserted Namnod as its first element and NODP as
 *   its second.  If a previously inserted node of the same namid
 *   already denotes an array of n elements, NODP becomes the
 *   n+1st element.
 */

#ifdef KSHELL
/* save code space */
void	linknod(nodp,root)
register struct Namnod *nodp;
register struct Amemory *root;
{
	register int i = chkid(nodp->namid);
	i &= root->memsize-1;
	nodp->namnxt = root->memhead[i];
	root->memhead[i] = nodp;
}
#else
void	linknod(nodp,root)
struct Namnod *nodp;
struct Amemory *root;
{
	register struct Namnod *np,*nq,**npp;
	struct Nodval *nv;
	struct Namaray *ap;
	int dot;
	char *cp = nodp->namid;
	int i = chkid(cp);

	i &= root->memsize-1;
	nodp->namnxt = NULL;
	for(npp= &root->memhead[i],np= *npp;np;npp= &np->namnxt,np= *npp)
		if(strcmp(cp,np->namid)==0)
		{
			if (!(attest (np, ARRAY)))
			{
				nq = mak_nod(cp);
				nq->namnxt = np->namnxt;
				*npp = nq;
				nq->value.namflg =  np->value.namflg|ARRAY;
				nq->value.namval.aray = ap = growaray((struct Namaray *)NULL,0);
				ap->val[0] = &np->value;
				nq->namsz = np->namsz;
				np = nq;
			}
			ap = arayp (np);
			dot = ++ap->adot;
			if (dot > ap->maxi)
				np->value.namval.aray = ap = growaray(ap,dot);
			if (nv = ap->val[dot])
				if (freeble (nv))
					rmnval (unmark (nv));
			ap->val[dot] = &nodp->value;
			return;
		}
	*npp = nodp;
}
#endif	/* KSHELL */


/*
 *   RMNVAL (NV)
 *
 *        struct Nodval *NV;
 *
 *   Remove freeable string space attached to NV, and then
 *   free the Nodval structure itself.
 *
 */

void	rmnval (nv)
struct Nodval *nv;
{
	register int flag = nv->namflg;
	register union Namval *up = &nv->namval;
	register char *cp;

	if (!(flag & N_FREE))
	{
		if (flag & IN_DIR)
		up = up->up;
		if (flag & INT_GER)
			if ((flag & L_FLAG) && (up->lp != NULL))
				free ((char*)(up->lp));
			else if ((cp = up->cp) != NULL)
				free (cp);
	}
	free ((char*)nv);
	return;
}
