/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Mike Olson.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)bt_stack.c	8.3 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>

#include <db.h>
#include "btree.h"

/*
 * When a page splits, a new record has to be inserted into its parent page.
 * This page may have to split as well, all the way up to the root.  Since
 * parent pointers in each page would be expensive, we maintain a stack of
 * parent pages as we descend the tree.
 *
 * XXX
 * This is a concurrency problem -- if user a builds a stack, then user b
 * splits the tree, then user a tries to split the tree, there's a new level
 * in the tree that user a doesn't know about.
 */

/*
 * __BT_PUSH -- Push parent page info onto the stack (LIFO).
 *
 * Parameters:
 *	t:	tree
 *	pgno:	page
 *	index:	page index
 *
 * Returns:
 * 	RET_ERROR, RET_SUCCESS
 */
int
__bt_push(t, pgno, index)
	BTREE *t;
	pgno_t pgno;
	indx_t index;
{
	if (t->bt_sp == t->bt_maxstack) {
		t->bt_maxstack += 50;
		if ((t->bt_stack = (EPGNO *)realloc(t->bt_stack,
		    t->bt_maxstack * sizeof(EPGNO))) == NULL) {
			t->bt_maxstack -= 50;
			return (RET_ERROR);
		}
	}

	t->bt_stack[t->bt_sp].pgno = pgno;
	t->bt_stack[t->bt_sp].index = index;
	++t->bt_sp;
	return (RET_SUCCESS);
}
