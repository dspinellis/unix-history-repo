/*-
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
static char sccsid[] = "@(#)yytree.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

#include "whoami.h"
#include "0.h"
#include "tree.h"
#include "tree_ty.h"

extern	int *spacep;

/*
 * LIST MANIPULATION ROUTINES
 *
 * The grammar for Pascal is written left recursively.
 * Because of this, the portions of parse trees which are to resemble
 * lists are in the somewhat inconvenient position of having
 * the nodes built from left to right, while we want to eventually
 * have as semantic value the leftmost node.
 * We could carry the leftmost node as semantic value, but this
 * would be inefficient as to add a new node to the list we would
 * have to chase down to the end.  Other solutions involving a head
 * and tail pointer waste space.
 *
 * The simple solution to this apparent dilemma is to carry along
 * a pointer to the leftmost node in a list in the rightmost node
 * which is the current semantic value of the list.
 * When we have completed building the list, we can retrieve this
 * left end pointer very easily; neither adding new elements to the list
 * nor finding the left end is thus expensive.  As the bottommost node
 * has an unused next pointer in it, no space is wasted either.
 *
 * The nodes referred to here are of the T_LISTPP type and have
 * the form:
 *
 *	T_LISTPP	some_pointer		next_element
 *
 * Here some_pointer points to the things of interest in the list,
 * and next_element to the next thing in the list except for the
 * rightmost node, in which case it points to the leftmost node.
 * The next_element of the rightmost node is of course zapped to the
 * NIL pointer when the list is completed.
 *
 * Thinking of the lists as tree we heceforth refer to the leftmost
 * node as the "top", and the rightmost node as the "bottom" or as
 * the "virtual root".
 */

/*
 * Make a new list
 */
struct tnode *
newlist(new)
	register struct tnode *new;
{

	if (new == TR_NIL)
		return (TR_NIL);
	return ((struct tnode *) tree3(T_LISTPP, (int) new,
					(struct tnode *) spacep));
}

/*
 * Add a new element to an existing list
 */
struct tnode *
addlist(vroot, new)
	register struct tnode *vroot;
	struct tnode *new;
{
	register struct tnode *top;

	if (new == TR_NIL)
		return (vroot);
	if (vroot == TR_NIL)
		return (newlist(new));
	top = vroot->list_node.next;
	vroot->list_node.next = (struct tnode *) spacep;
	return ((struct tnode *) tree3(T_LISTPP, (int) new, top));
}

/*
 * Fix up the list which has virtual root vroot.
 * We grab the top pointer and return it, zapping the spot
 * where it was so that the tree is not circular.
 */
struct tnode *
fixlist(vroot)
	register struct tnode *vroot;
{
	register struct tnode *top;

	if (vroot == TR_NIL)
		return (TR_NIL);
	top = vroot->list_node.next;
	vroot->list_node.next = TR_NIL;
	return (top);
}


/*
 * Set up a T_VAR node for a qualified variable.
 * Init is the initial entry in the qualification,
 * or NIL if there is none.
 *
 * if we are building pTrees, there has to be an extra slot for 
 * a pointer to the namelist entry of a field, if this T_VAR refers
 * to a field name within a WITH statement.
 * this extra field is set in lvalue, and used in VarCopy.
 */
struct tnode *
setupvar(var, init)
	char *var;
	register struct tnode *init;
{

	if (init != TR_NIL)
		init = newlist(init);
#	ifndef PTREE
	    return (tree4(T_VAR, NOCON, (struct tnode *) var, init));
#	else
	    return tree5( T_VAR, NOCON, (struct tnode *) var, init, TR_NIL );
#	endif
}

    /*
     *	set up a T_TYREC node for a record
     *
     *	if we are building pTrees, there has to be an extra slot for 
     *	a pointer to the namelist entry of the record.
     *	this extra field is filled in in gtype, and used in RecTCopy.
     */
struct tnode *
setuptyrec( line , fldlist )
    int	line;
    struct tnode *fldlist;
    {

#	ifndef PTREE
	    return tree3( T_TYREC , line , fldlist );
#	else
	    return tree4( T_TYREC , line , fldlst , TR_NIL );
#	endif
    }

    /*
     *	set up a T_FIELD node for a field.
     *
     *	if we are building pTrees, there has to be an extra slot for
     *	a pointer to the namelist entry of the field.
     *	this extra field is set in lvalue, and used in SelCopy.
     */
struct tnode *
setupfield( field , other )
    struct tnode *field;
    struct tnode *other;
    {

#	ifndef PTREE
	    return tree3( T_FIELD , (int) field , other );
#	else
	    return tree4( T_FIELD , (int) field , other , TR_NIL );
#	endif
    }
