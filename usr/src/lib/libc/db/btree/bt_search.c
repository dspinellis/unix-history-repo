/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Mike Olson.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)bt_search.c	5.7 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>

#include <db.h>
#include <stdio.h>

#include "btree.h"

/*
 * __BT_SEARCH -- Search a btree for a key.
 *
 * Parameters:
 *	t:	tree to search
 *	key:	key to find
 *	exactp:	pointer to exact match flag
 *
 * Returns:
 *	EPG for matching record, if any, or the EPG for the location of the
 *	key, if it were inserted into the tree.
 *
 * Warnings:
 *	The EPG returned is in static memory, and will be overwritten by the
 *	next search of any kind in any tree.
 */
EPG *
__bt_search(t, key, exactp)
	BTREE *t;
	const DBT *key;
	int *exactp;
{
	register index_t index;
	register int base, cmp, lim;
	register PAGE *h;
	pgno_t pg;
	static EPG e;

	BT_CLR(t);
	for (pg = P_ROOT;;) {
		if ((h = mpool_get(t->bt_mp, pg, 0)) == NULL)
			return (NULL);

		/* Do a binary search on the current page. */
		e.page = h;
		for (base = 0, lim = NEXTINDEX(h); lim; lim >>= 1) {
			e.index = index = base + (lim >> 1);
			if ((cmp = __bt_cmp(t, key, &e)) == 0) {
				if (h->flags & P_BLEAF) {
					*exactp = 1;
					return (&e);
				}
				goto next;
			}
			if (cmp > 0) {
				base = index + 1;
				--lim;
			}
		}

		/* If it's a leaf page, we're done. */
		if (h->flags & P_BLEAF) {
			e.index = base;
			*exactp = 0;
			return (&e);
		}

		/*
		 * No match found.  Base is the smallest index greater than
		 * key and may be zero or a last + 1 index.  If it's non-zero,
		 * decrement by one, and record the internal page which should
		 * be a parent page for the key.  If a split later occurs, the
		 * inserted page will be to the right of the saved page.
		 */
		index = base ? base - 1 : base;

next:		if (__bt_push(t, h->pgno, index) == RET_ERROR)
			return (NULL);
		pg = GETBINTERNAL(h, index)->pgno;
		mpool_put(t->bt_mp, h, 0);
	}
}
