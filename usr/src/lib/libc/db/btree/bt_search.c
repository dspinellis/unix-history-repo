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
static char sccsid[] = "@(#)bt_search.c	5.5 (Berkeley) %G%";
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

		/*
		 * No match found.  Base is the smallest index greater than
		 * key but may be an illegal index.  Use base if it's a leaf
		 * page, decrement it by one if it's an internal page.  This
		 * is safe because internal pages can't be empty.
		 */
		index = h->flags & P_BLEAF ? base : base - 1;

		/* If it's a leaf page, we're done. */
		if (h->flags & P_BLEAF) {
			e.index = index;
			*exactp = 0;
			return (&e);
		}

next:		if (__bt_push(t, h->pgno, index) == RET_ERROR)
			return (NULL);
		pg = GETBINTERNAL(h, index)->pgno;
		mpool_put(t->bt_mp, h, 0);
	}
}
