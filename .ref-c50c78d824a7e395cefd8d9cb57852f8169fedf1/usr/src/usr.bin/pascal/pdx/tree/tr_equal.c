/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)tr_equal.c	5.1 (Berkeley) %G%";
#endif not lint

/*
 * A recursive tree search routine to test if two trees
 * are structurally equivalent.
 */

#include "defs.h"
#include "tree.h"
#include "tree.rep"

BOOLEAN tr_equal(t1, t2)
register NODE *t1;
register NODE *t2;
{
	if (t1 == NIL && t2 == NIL) {
		return(TRUE);
	}
	if (t1 == NIL || t2 == NIL) {
		return(FALSE);
	}
	if (t1->op != t2->op || degree(t1->op) != degree(t2->op)) {
		return(FALSE);
	}
	switch(degree(t1->op)) {
		case LEAF:
			switch(t1->op) {
				case O_NAME:
					return(t1->nameval == t2->nameval);

				case O_QNAME:
					if (!tr_equal(t1->right, t2->right)) {
						return(FALSE);
					}
					return(tr_equal(t1->left, t2->left));

				case O_LCON:
					return(t1->lconval == t2->lconval);

				case O_FCON:
					return(t1->fconval == t2->fconval);

				case O_SCON:
					return(t1->sconval == t2->sconval);

				default:
					panic("tr_equal: leaf %d\n", t1->op);
			}
			/*NOTREACHED*/

		case BINARY:
			if (!tr_equal(t1->right, t2->right)) {
				return(FALSE);
			}
			/* else fall through */
		case UNARY:
			return(tr_equal(t1->left, t2->left));

		default:
			panic("tr_equal: bad degree for op %d\n", t1->op);
	}
	/*NOTREACHED*/
}
