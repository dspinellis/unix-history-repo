/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/* @(#)unassign.c	1.1 */

/*
 *   UNASSIGN.C
 *
 *   Programmer:  D. G. Korn
 *
 *        Owner:  D. A. Lambeth
 *
 *         Date:  April 17, 1980
 *
 *
 *
 *   UNASSIGN (NODE)
 *        
 *        Nullify the value and the attributes of the Namnod
 *        given by NODE.
 *
 *
 *
 *   See Also:  assign(III), assnum(III), assiadr(III), asscadr(III),
 *              valup(III)
 */

#include	"name.h"
#include        "flags.h"


/*
 *   UNASSIGN (NODE)
 *
 *       struct Namnod *NODE;
 * 
 *   Set the value of NODE to NULL, and nullify any attributes
 *   that NODE may have had.  Free any freeable space occupied
 *   by the value of NODE.  If NODE denotes an array member, it
 *   will retain its attributes.  Any node that has the
 *   indirect (IN_DIR) attribute will retain that attribute.
 */

extern void	free();

void	unassign(node)
struct Namnod *node;
{
	register struct Namnod *np=node;
	register union Namval *up = &np->value.namval;
#ifdef NAME_SCOPE
	if (attest (np, C_WRITE))
	{
		np->value.namflg |= N_AVAIL;
		return;
	}
#endif
	if (attest (np, ARRAY))
	{
		register struct Namaray *ap = up->aray;
		if(ap->adot != NO_SUBSCRIPT)
		{
			struct Nodval *nv = unmark(ap->val[ap->adot]);
			if(ap->adot == ap->maxi)
				ap->maxi--;
			if(nv==NULL || (up = &nv->namval)==NULL)
				return;
		}
		else
		{
			for(ap->adot=0;ap->adot <= ap->maxi;ap->adot++)
				unassign(np);
			free((char*)ap);
			up->cp = NULL;
			np->value.namflg = 0;
			return;
		}
	}
	if (attest (np, IN_DIR))
		up = up->up;
	if (attest (np, INT_GER))
	{
        	if ((attest (np, L_FLAG)) && (up->lp != NULL))
			free((char *)up->lp);
	}
	else if ((!attest (np, N_FREE)) && (!isnull (np)))
		free(up->cp);
	up->cp = NULL;
	if (!attest (np, ARRAY))
	{
		np->value.namflg &= IN_DIR;
		np->namsz = 0;
	}
}
