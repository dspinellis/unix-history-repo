/*-
 * lstInt.h --
 *	Internals for the list library
 *
 * Copyright (c) 1988 by University of California Regents
 *
 * Permission to use, copy, modify, and distribute this
 * software and its documentation for any purpose and without
 * fee is hereby granted, provided that the above copyright
 * notice appears in all copies.  Neither the University of California nor
 * Adam de Boor makes any representations about the suitability of this
 * software for any purpose.  It is provided "as is" without
 * express or implied warranty.
 *
 * $Id: lstInt.h,v 1.11 89/06/13 15:01:46 adam Exp $ SPRITE (Berkeley)
 */
#ifndef _LSTINT_H_
#define _LSTINT_H_

#include	  "lst.h"

typedef struct ListNode {
	struct ListNode	*prevPtr;   /* previous element in list */
	struct ListNode	*nextPtr;   /* next in list */
	short	    	useCount:8, /* Count of functions using the node.
				     * node may not be deleted until count
				     * goes to 0 */
 	    	    	flags:8;    /* Node status flags */
	ClientData	datum;	    /* datum associated with this element */
} *ListNode;
/*
 * Flags required for synchronization
 */
#define LN_DELETED  	0x0001      /* List node should be removed when done */

#define NilListNode	((ListNode)-1)

typedef enum {
    Head, Middle, Tail, Unknown
} Where;

typedef struct	{
	ListNode  	firstPtr; /* first node in list */
	ListNode  	lastPtr;  /* last node in list */
	Boolean	  	isCirc;	  /* true if the list should be considered
				   * circular */
/*
 * fields for sequential access
 */
	Where	  	atEnd;	  /* Where in the list the last access was */
	Boolean	  	isOpen;	  /* true if list has been Lst_Open'ed */
	ListNode  	curPtr;	  /* current node, if open. NilListNode if
				   * *just* opened */
	ListNode  	prevPtr;  /* Previous node, if open. Used by
				   * Lst_Remove */
} *List;

#define NilList	  	((List)-1)

/*
 * PAlloc (var, ptype) --
 *	Allocate a pointer-typedef structure 'ptype' into the variable 'var'
 */
#define	PAlloc(var,ptype)	var = (ptype) Malloc (sizeof (*var))

/*
 * LstValid (l) --
 *	Return TRUE if the list l is valid
 */
#define LstValid(l)	(((Lst)l == NILLST) ? FALSE : TRUE)

/*
 * LstNodeValid (ln, l) --
 *	Return TRUE if the LstNode ln is valid with respect to l
 */
#define LstNodeValid(ln, l)	((((LstNode)ln) == NILLNODE) ? FALSE : TRUE)

/*
 * LstIsEmpty (l) --
 *	TRUE if the list l is empty.
 */
#define LstIsEmpty(l)	(((List)l)->firstPtr == NilListNode)

#endif _LSTINT_H_
