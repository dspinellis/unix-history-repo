/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/* @(#)valup.c	1.1 */

/*
 *   VALUP.C
 *
 *   Programmer:  D. G. Korn
 *
 *        Owner:  D. A. Lambeth
 *
 *         Date:  April 17, 1980
 *
 *
 *
 *   VALUP (NODP)
 *        
 *        Return a pointer to the value of the node given by NODP.
 *
 *
 *
 *   See Also:  assign(III), asscadr(III), assiadr(III), assnum(III),
 *              unassign(III)
 */

#include	"name.h"
#include        "flags.h"

#ifdef NAME_SCOPE
extern struct Namnod *copy_nod();
#endif
extern char *utos();
extern char *ltos();
extern char *itos();
extern void failed();


/*
 *   VALUP (NODP)
 *       
 *        struct Namnod *NODP;
 *
 *   Return a pointer to a character string that denotes the value
 *   of NODP.  If NODP refers to an array,  return a pointer to
 *   the value associated with the current index.
 *
 *   If NODP is blocked, N_AVAIL, then the value of the node
 *   with the same name in the last tree is returned.
 *   Thus a node can become blocked after the lookup but
 *   before retrieving its value and still work correctly.
 *
 *   If the value of NODP is an integer, the string returned will
 *   be overwritten by the next call to valup.
 *
 *   If NODP has no value, NULL is returned.
 */

char *valup(nodp)
struct Namnod *nodp;
{
	int dot;
	register struct Namnod *np=nodp;
	register union Namval *up= &np->value.namval;
	register struct Nodval *nv;
#ifdef NAME_SCOPE
	if (attest (np,N_AVAIL))	/* node blocked */
		/* use node with same name in last tree */
		np = copy_nod(np,0);
#endif
	if (attest (np, ARRAY))
        {
        	dot = up->aray->adot;
        	if (dot > abound (np))
        		failed (itos(dot), subscript);
        	if ((nv = up->aray->val[dot]) == NULL)
	        	return (NULL);
        	else
	        	up = &(unmark (nv)->namval);
        }
	if ((attest (np, IN_DIR)) && up->cp)
		up = up->up;
	if (attest (np, INT_GER))
	{
		long l;
        	if (attest (np, CPOIN_TER))
			return(up->cp);
		else if(attest (np, (BLT_NOD)))
			l = ((*up->fp->f_vp)());
        	else
		{
			if(up->lp == NULL)
				return(NULL);
			l = *(up->lp);
		}
		if (attest (np, (BLT_NOD|UN_SIGN)))
#ifdef pdp11
			return(utos(l,np->namsz));
#else
			return(utos((unsigned long)l,np->namsz));
#endif /* pdp11 */
		return(ltos(l,np->namsz));
	}
	return (up->cp);
}

