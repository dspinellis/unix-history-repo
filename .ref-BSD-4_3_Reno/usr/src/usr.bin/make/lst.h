/*
 * Copyright (c) 1988, 1989, 1990 The Regents of the University of California.
 * Copyright (c) 1988, 1989 by Adam de Boor
 * Copyright (c) 1989 by Berkeley Softworks
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
 *	@(#)lst.h	5.3 (Berkeley) 6/1/90
 */

/*-
 * lst.h --
 *	Header for using the list library
 */
#ifndef _LST_H_
#define _LST_H_

#include	<sprite.h>

/*
 * basic typedef. This is what the Lst_ functions handle
 */

typedef	struct	Lst	*Lst;
typedef	struct	LstNode	*LstNode;

#define	NILLST		((Lst) NIL)
#define	NILLNODE	((LstNode) NIL)

/*
 * NOFREE can be used as the freeProc to Lst_Destroy when the elements are
 *	not to be freed.
 * NOCOPY performs similarly when given as the copyProc to Lst_Duplicate.
 */
#define NOFREE		((void (*)()) 0)
#define NOCOPY		((ClientData (*)()) 0)

#define LST_CONCNEW	0   /* create new LstNode's when using Lst_Concat */
#define LST_CONCLINK	1   /* relink LstNode's when using Lst_Concat */

/*
 * Creation/destruction functions
 */
Lst		  Lst_Init();	    	/* Create a new list */
Lst	    	  Lst_Duplicate();  	/* Duplicate an existing list */
void		  Lst_Destroy();	/* Destroy an old one */

int	    	  Lst_Length();	    	/* Find the length of a list */
Boolean		  Lst_IsEmpty();	/* True if list is empty */

/*
 * Functions to modify a list
 */
ReturnStatus	  Lst_Insert();	    	/* Insert an element before another */
ReturnStatus	  Lst_Append();	    	/* Insert an element after another */
ReturnStatus	  Lst_AtFront();    	/* Place an element at the front of
					 * a lst. */
ReturnStatus	  Lst_AtEnd();	    	/* Place an element at the end of a
					 * lst. */
ReturnStatus	  Lst_Remove();	    	/* Remove an element */
ReturnStatus	  Lst_Replace();	/* Replace a node with a new value */
ReturnStatus	  Lst_Move();	    	/* Move an element to another place */
ReturnStatus	  Lst_Concat();	    	/* Concatenate two lists */

/*
 * Node-specific functions
 */
LstNode		  Lst_First();	    	/* Return first element in list */
LstNode		  Lst_Last();	    	/* Return last element in list */
LstNode		  Lst_Succ();	    	/* Return successor to given element */
LstNode		  Lst_Pred();	    	/* Return predecessor to given
					 * element */
ClientData	  Lst_Datum();	    	/* Get datum from LstNode */

/*
 * Functions for entire lists
 */
LstNode		  Lst_Find();	    	/* Find an element in a list */
LstNode		  Lst_FindFrom();	/* Find an element starting from
					 * somewhere */
LstNode	    	  Lst_Member();	    	/* See if the given datum is on the
					 * list. Returns the LstNode containing
					 * the datum */
int	    	  Lst_Index();	    	/* Returns the index of a datum in the
					 * list, starting from 0 */
void		  Lst_ForEach();	/* Apply a function to all elements of
					 * a lst */
void	    	  Lst_ForEachFrom();  	/* Apply a function to all elements of
					 * a lst starting from a certain point.
					 * If the list is circular, the
					 * application will wrap around to the
					 * beginning of the list again. */
/*
 * these functions are for dealing with a list as a table, of sorts.
 * An idea of the "current element" is kept and used by all the functions
 * between Lst_Open() and Lst_Close().
 */
ReturnStatus	  Lst_Open();	    	/* Open the list */
LstNode		  Lst_Prev();	    	/* Previous element */
LstNode		  Lst_Cur();	    	/* The current element, please */
LstNode		  Lst_Next();	    	/* Next element please */
Boolean		  Lst_IsAtEnd();	/* Done yet? */
void		  Lst_Close();	    	/* Finish table access */

/*
 * for using the list as a queue
 */
ReturnStatus	  Lst_EnQueue();	/* Place an element at tail of queue */
ClientData	  Lst_DeQueue();	/* Remove an element from head of
					 * queue */

#endif _LST_H_
