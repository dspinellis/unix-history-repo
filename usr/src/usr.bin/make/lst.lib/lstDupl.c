/*-
 * listDupl.c --
 *	Duplicate a list. This includes duplicating the individual
 *	elements.
 *
 * Copyright (c) 1988 by the Regents of the University of California
 *
 */
#ifndef lint
static char *rcsid =
"$Id: lstDupl.c,v 1.4 88/11/17 20:52:21 adam Exp $ SPRITE (Berkeley)";
#endif lint

#include    "lstInt.h"

/*-
 *-----------------------------------------------------------------------
 * Lst_Duplicate --
 *	Duplicate an entire list. If a function to copy a ClientData is
 *	given, the individual client elements will be duplicated as well.
 *
 * Results:
 *	The new Lst structure or NILLST if failure.
 *
 * Side Effects:
 *	A new list is created.
 *-----------------------------------------------------------------------
 */
Lst
Lst_Duplicate (l, copyProc)
    Lst     	  l;	    	 /* the list to duplicate */
    ClientData	  (*copyProc)(); /* A function to duplicate each ClientData */
{
    register Lst 	nl;
    register ListNode  	ln;
    register List 	list = (List)l;
    
    if (!LstValid (l)) {
	return (NILLST);
    }

    nl = Lst_Init (list->isCirc);
    if (nl == NILLST) {
	return (NILLST);
    }

    ln = list->firstPtr;
    while (ln != NilListNode) {
	if (copyProc != NOCOPY) {
	    if (Lst_AtEnd (nl, (*copyProc) (ln->datum)) == FAILURE) {
		return (NILLST);
	    }
	} else if (Lst_AtEnd (nl, ln->datum) == FAILURE) {
	    return (NILLST);
	}

	if (list->isCirc && ln == list->lastPtr) {
	    ln = NilListNode;
	} else {
	    ln = ln->nextPtr;
	}
    }
	
    return (nl);
}
