/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/* @(#)assnum.c	1.1 */

/*
 *   ASSNUM.C
 *
 *   Programmer:  D. G. Korn
 *
 *        Owner:  D. A. Lambeth
 *
 *         Date:  April 17, 1980
 *
 *
 *
 *
 *   ASSCADR (NODE, CHARP)
 *
 *        Assign the string address CHARP to the Namnod given
 *        by node.
 *
 *   ASSLONG (NODE, NUM)
 *
 *        Assign the long integer NUM to NODE.  NODE should have
 *        the L_FLAG and INT_GER attributes.
 *
 *
 *
 *   See Also:  assign(III), unassign(III), valup(III), ltos(III)
 */

#include	"name.h"
#include        "flags.h"

extern char *ltos(), *malloc();
#ifdef NAME_SCOPE
extern struct Namnod *copy_nod();
#endif
extern union Namval *aget_up();
extern void	assign();


/*
 *   ASSCADR (NODE, CHARP)
 *
 *        struct Namnod *NODE;
 *
 *        char *CHARP;
 *
 *   Assign the string address CHARP to the Namnod given by NODE.
 *   The attributes of NODE are neither inspected nor altered.
 *   Thus, to guarantee success, NODE should have the N_DEFAULT
 *   value-determining attribute.
 *   The following code has been replaced by a macro in name.h.

asscadr(node,charp)
struct Namnod *node;
char *charp;
{
	node->value.namval.cp = charp;
}
 */

/*
 *   ASSLONG (NODE, NUM)
 *
 *        struct Namnod *NODE;
 *
 *        int NUM;
 *
 *   Assign the value NUM to the Namnod given by NODE.  All 
 *   appropriate conversions are made.
 */

asslong(node,num)
struct Namnod *node;
long num;
{
	register struct Namnod *np=node;
	register union Namval *up = &np->value.namval;
	if (attest (np, INT_GER))
	{
		if (attest (np, ARRAY))
			up = aget_up(np,up);
#ifdef NAME_SCOPE
		if (attest (np, C_WRITE))
			np = copy_nod(np,1);
#endif	/* NAME_SCOPE */
        	if (attest (np, IN_DIR))
			up = up->up;
        	if (attest (np, BLT_NOD))
			(*up->fp->f_ap)((unsigned)num);
		else
		{
			if(up->lp==0)
				up->lp = (long*)malloc((unsigned)sizeof(long));
			*(up->lp) = num;
			if(np->namsz == 0)
				np->namsz = lastbase;
		}
	}
	else
		assign(np,ltos(num,10));
}
