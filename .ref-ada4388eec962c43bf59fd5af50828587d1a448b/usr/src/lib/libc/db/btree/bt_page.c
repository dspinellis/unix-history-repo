/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)bt_page.c	8.2 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>

#include <stdio.h>

#include <db.h>
#include "btree.h"

/*
 * __BT_FREE -- Put a page on the freelist.
 *
 * Parameters:
 *	t:	tree
 *	h:	page to free
 *
 * Returns:
 *	RET_ERROR, RET_SUCCESS
 */
int
__bt_free(t, h)
	BTREE *t;
	PAGE *h;
{
	/* Insert the page at the start of the free list. */
	h->prevpg = P_INVALID;
	h->nextpg = t->bt_free;
	t->bt_free = h->pgno;

	/* Make sure the page gets written back. */
	return (mpool_put(t->bt_mp, h, MPOOL_DIRTY));
}

/*
 * __BT_NEW -- Get a new page, preferably from the freelist.
 *
 * Parameters:
 *	t:	tree
 *	npg:	storage for page number.
 *
 * Returns:
 *	Pointer to a page, NULL on error.
 */
PAGE *
__bt_new(t, npg)
	BTREE *t;
	pgno_t *npg;
{
	PAGE *h;

	if (t->bt_free != P_INVALID &&
	    (h = mpool_get(t->bt_mp, t->bt_free, 0)) != NULL) {
			*npg = t->bt_free;
			t->bt_free = h->nextpg;
			return (h);
	}
	return (mpool_new(t->bt_mp, npg));
}
