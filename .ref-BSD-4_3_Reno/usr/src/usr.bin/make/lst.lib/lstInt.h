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
 *
 *	@(#)lstInt.h	5.3 (Berkeley) 6/1/90
 */

/*-
 * lstInt.h --
 *	Internals for the list library
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
