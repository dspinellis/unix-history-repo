/*
 * Copyright (c) 1988, 1989, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Adam de Boor.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)lstDestroy.c	5.3 (Berkeley) 6/1/90";
#endif /* not lint */

/*-
 * LstDestroy.c --
 *	Nuke a list and all its resources
 */

#include	"lstInt.h"

/*-
 *-----------------------------------------------------------------------
 * Lst_Destroy --
 *	Destroy a list and free all its resources. If the freeProc is
 *	given, it is called with the datum from each node in turn before
 *	the node is freed.
 *
 * Results:
 *	None.
 *
 * Side Effects:
 *	The given list is freed in its entirety.
 *
 *-----------------------------------------------------------------------
 */
void
Lst_Destroy (l, freeProc)
    Lst	    	  	l;
    register void	(*freeProc)();
{
    register ListNode	ln;
    register ListNode	tln = NilListNode;
    register List 	list = (List)l;
    
    if (l == NILLST || ! l) {
	/*
	 * Note the check for l == (Lst)0 to catch uninitialized static Lst's.
	 * Gross, but useful.
	 */
	return;
    }
    
    if (freeProc) {
	for (ln = list->firstPtr;
	     ln != NilListNode && tln != list->firstPtr;
	     ln = tln) {
		 tln = ln->nextPtr;
		 (*freeProc) (ln->datum);
		 free ((Address)ln);
	}
    } else {
	for (ln = list->firstPtr;
	     ln != NilListNode && tln != list->firstPtr;
	     ln = tln) {
		 tln = ln->nextPtr;
		 free ((Address)ln);
	}
    }
    
    free ((Address)l);
}
