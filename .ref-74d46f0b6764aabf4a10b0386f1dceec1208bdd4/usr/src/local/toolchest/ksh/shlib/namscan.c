/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/* @(#)namscan.c	1.1 */

/*
 *   NAMSCAN.C
 *
 *   GSCAN_ALL (FN, ROOT)
 *        Execute FN at each node in the linked memory trees,
 *        which are given by ROOT.
 *
 *   GSCAN_SOME (FN, ROOT, MASK, FLAG)
 *        Execute FN at those nodes in the linked memory trees
 *        that have certain attributes, as determined by MASK and
 *        FLAG. ROOT is the first of the list of memory trees.
 *
 *   SCAN_ALL (FN, ROOT)
 *        Execute function FN at each of the Namnods in the tree
 *        given by ROOT.
 *
 */

#include	"flags.h"
#include	"name.h"

/* These routines are defined by this module */
void	gscan_all();
void	scan_all();
void	gscan_some();

static int scanmask;
static int scanflag;


/*
 *   GSCAN_ALL (FN, ROOT)
 *   
 *        int (*FN)();
 *
 *	  struct Amemory *root;
 *
 *   Execute FN at each node in the linked memory trees. 
 *   Note that the first tree need not exist.
 */

void	gscan_all(fn, root)
void	(*fn)();
struct Amemory *root;
{
	register struct Amemory *app = root;
	while(app)
	{
		scan_all(fn,app);
		app = app->nexttree;
	}
	return;
}


/*
 *   GSCAN_SOME (FN, ROOT, MASK, FLAG)
 *        int (*FN)();
 *        struct Amemory *ROOT;
 *        int MASK;
 *        int FLAG;
 *
 *   Execute FN at each of the Namnods in the trees given by ROOT
 *   that meet certain criteria, as determined by MASK and FLAG.
 *   If flag is non-zero then at least one of these mask bits must be on.
 *   If flag is zero then all the mask bits must be off to match.
 */

void	gscan_some(fn,root,mask,flag)
void (*fn)();
int mask,flag;
struct Amemory *root;
{
	scanmask = mask;
	scanflag = flag;
	gscan_all(fn,root);
	scanmask = scanflag = 0;
}

/*
 *   SCAN_ALL (FN, ROOT)
 *        int (*FN)();
 *        struct Amemory *ROOT;
 *
 *   Execute FN at each node in the tree given by ROOT, according
 *   to the values of scanmask and scanflag, which are established
 *   in scan_some().
 */

void	scan_all(fn,root)
void (*fn)();
struct Amemory *root;
{
	register struct Namnod *np;
	register int i;
	register int smask = scanmask^N_AVAIL;
	int k;
	for(i=0;i < root->memsize;i++)
		for(np=root->memhead[i];np;np= np->namnxt)
		{
			k = np->value.namflg&smask;
			if((scanflag?scanflag&k:k==0))
			{
				if (attest (np, ARRAY))
				{
					register struct Namaray *ap=arayp(np);
					if(ap == NULL)
						(*fn)(np);
 					else
					{
						int i = ap->adot;
						for (ap->adot=i=0; i<=ap->maxi;i++)
						{
							ap->adot = i;
							if (ap->val[i])
								(*fn)(np);
							i = ap->adot;
						}
					}
				}
				else if (attest(np,~N_DEFAULT) || np->value.namval.cp)
					(*fn)(np);
			}
		}
}

