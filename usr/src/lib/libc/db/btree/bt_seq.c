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
static char sccsid[] = "@(#)bt_seq.c	5.7 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>

#include <db.h>
#include <errno.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

#include "btree.h"

static int	 bt_seqadv __P((BTREE *, EPG *, int));
static int	 bt_seqset __P((BTREE *, EPG *, DBT *, int));

/*
 * Sequential scan support.
 *
 * The tree can be scanned sequentially, starting from either end of the tree
 * or from any specific key.  A scan request before any scanning is done is
 * initialized as starting from the least node.
 *
 * Each tree has an EPGNO which has the current position of the cursor.  The
 * cursor has to survive deletions/insertions in the tree without losing its
 * position.  This is done by noting deletions without doing them, and then
 * doing them when the cursor moves (or the tree is closed).
 */

/*
 * __BT_SEQ -- Btree sequential scan interface.
 *
 * Parameters:
 *	dbp:	pointer to access method
 *	key:	key for positioning and return value
 *	data:	data return value
 *	flags:	R_CURSOR, R_FIRST, R_LAST, R_NEXT, R_PREV.
 *
 * Returns:
 *	RET_ERROR, RET_SUCCESS or RET_SPECIAL if there's no next key.
 */
int
__bt_seq(dbp, key, data, flags)
	const DB *dbp;
	DBT *key, *data;
	u_int flags;
{
	BTREE *t;
	EPG e;
	int status;

	/*
	 * If scan unitialized as yet, or starting at a specific record, set
	 * the scan to a specific key.  Both bt_seqset and bt_seqadv pin the
	 * page the cursor references if they're successful.
	 */
	t = dbp->internal;
	switch(flags) {
	case R_NEXT:
	case R_PREV:
		if (ISSET(t, BTF_SEQINIT)) {
			status = bt_seqadv(t, &e, flags);
			break;
		}
		/* FALLTHROUGH */
	case R_CURSOR:
	case R_FIRST:
	case R_LAST:
		status = bt_seqset(t, &e, key, flags);
		break;
	default:
		errno = EINVAL;
		return (RET_ERROR);
	}

	if (status == RET_SUCCESS) {
		status = __bt_ret(t, &e, key, data);

		/* Update the actual cursor. */
		t->bt_bcursor.pgno = e.page->pgno;
		t->bt_bcursor.index = e.index;
		mpool_put(t->bt_mp, e.page, 0);
		SET(t, BTF_SEQINIT);
	}
	return (status);
}

/*
 * BT_SEQSET -- Set the sequential scan to a specific key.
 *
 * Parameters:
 *	t:	tree
 *	ep:	storage for returned key
 *	key:	key for initial scan position
 *	flags:	R_CURSOR, R_FIRST, R_LAST, R_NEXT, R_PREV
 *
 * Side effects:
 *	Pins the page the cursor references.
 *
 * Returns:
 *	RET_ERROR, RET_SUCCESS or RET_SPECIAL if there's no next key.
 */
static int
bt_seqset(t, ep, key, flags)
	BTREE *t;
	EPG *ep;
	DBT *key;
	int flags;
{
	EPG *e;
	PAGE *h;
	pgno_t pg;
	int exact;

	/*
	 * Delete any already deleted record that we've been saving because
	 * the cursor pointed to it.  Since going to a specific key, should
	 * delete any logically deleted records so they aren't found.
	 */
	if (ISSET(t, BTF_DELCRSR) && __bt_crsrdel(t, &t->bt_bcursor))
		return (RET_ERROR);

	/*
	 * Find the first, last or specific key in the tree and point the cursor
	 * at it.  The cursor may not be moved until a new key has been found.
	 */
	switch(flags) {
	case R_CURSOR:				/* Keyed scan. */
		/*
		 * Find the first instance of the key or the smallest key which
		 * is greater than or equal to the specified key.  If run out
		 * of keys, return RET_SPECIAL.
		 */
		if (key->data == NULL || key->size == 0) {
			errno = EINVAL;
			return (RET_ERROR);
		}
		e = __bt_first(t, key, &exact);	/* Returns pinned page. */
		if (e == NULL)
			return (RET_ERROR);
		/*
		 * If at the end of a page, skip any empty pages and find the
		 * next entry.
		 */
		if (e->index == NEXTINDEX(e->page)) {
			h = e->page;
			do {
				pg = h->nextpg;
				mpool_put(t->bt_mp, h, 0);
				if (pg == P_INVALID)
					return (RET_SPECIAL);
				if ((h = mpool_get(t->bt_mp, pg, 0)) == NULL)
					return (RET_ERROR);
			} while (NEXTINDEX(h) == 0);
			e->index = 0;
			e->page = h;
		}
		*ep = *e;
		break;
	case R_FIRST:				/* First record. */
	case R_NEXT:
		/* Walk down the left-hand side of the tree. */
		for (pg = P_ROOT;;) {
			if ((h = mpool_get(t->bt_mp, pg, 0)) == NULL)
				return (RET_ERROR);
			if (h->flags & (P_BLEAF | P_RLEAF))
				break;
			pg = GETBINTERNAL(h, 0)->pgno;
			mpool_put(t->bt_mp, h, 0);
		}

		/* Skip any empty pages. */
		while (NEXTINDEX(h) == 0 && h->nextpg != P_INVALID) {
			pg = h->nextpg;
			mpool_put(t->bt_mp, h, 0);
			if ((h = mpool_get(t->bt_mp, pg, 0)) == NULL)
				return (RET_ERROR);
		}

		if (NEXTINDEX(h) == 0) {
			mpool_put(t->bt_mp, h, 0);
			return (RET_SPECIAL);
		}

		ep->page = h;
		ep->index = 0;
		break;
	case R_LAST:				/* Last record. */
	case R_PREV:
		/* Walk down the right-hand side of the tree. */
		for (pg = P_ROOT;;) {
			if ((h = mpool_get(t->bt_mp, pg, 0)) == NULL)
				return (RET_ERROR);
			if (h->flags & (P_BLEAF | P_RLEAF))
				break;
			pg = GETBINTERNAL(h, NEXTINDEX(h) - 1)->pgno;
			mpool_put(t->bt_mp, h, 0);
		}

		/* Skip any empty pages. */
		while (NEXTINDEX(h) == 0 && h->prevpg != P_INVALID) {
			pg = h->prevpg;
			mpool_put(t->bt_mp, h, 0);
			if ((h = mpool_get(t->bt_mp, pg, 0)) == NULL)
				return (RET_ERROR);
		}

		if (NEXTINDEX(h) == 0) {
			mpool_put(t->bt_mp, h, 0);
			return (RET_SPECIAL);
		}

		ep->page = h;
		ep->index = NEXTINDEX(h) - 1;
		break;
	}
	return (RET_SUCCESS);
}

/*
 * BT_SEQADVANCE -- Advance the sequential scan.
 *
 * Parameters:
 *	t:	tree
 *	flags:	R_NEXT, R_PREV
 *
 * Side effects:
 *	Pins the page the new key/data record is on.
 *
 * Returns:
 *	RET_ERROR, RET_SUCCESS or RET_SPECIAL if there's no next key.
 */
static int
bt_seqadv(t, e, flags)
	BTREE *t;
	EPG *e;
	int flags;
{
	EPGNO *c, delc;
	PAGE *h;
	index_t index;
	pgno_t pg;

	/* Save the current cursor if going to delete it. */
	c = &t->bt_bcursor;
	if (ISSET(t, BTF_DELCRSR))
		delc = *c;

	if ((h = mpool_get(t->bt_mp, c->pgno, 0)) == NULL)
		return (RET_ERROR);

	/*
 	 * Find the next/previous record in the tree and point the cursor at it.
	 * The cursor may not be moved until a new key has been found.
	 */
	index = c->index;
	switch(flags) {
	case R_NEXT:			/* Next record. */
		if (++index == NEXTINDEX(h)) {
			do {
				pg = h->nextpg;
				mpool_put(t->bt_mp, h, 0);
				if (pg == P_INVALID)
					return (RET_SPECIAL);
				if ((h = mpool_get(t->bt_mp, pg, 0)) == NULL)
					return (RET_ERROR);
			} while (NEXTINDEX(h) == 0);
			index = 0;
		}
		break;
	case R_PREV:			/* Previous record. */
		if (index-- == 0) {
			do {
				pg = h->prevpg;
				mpool_put(t->bt_mp, h, 0);
				if (pg == P_INVALID)
					return (RET_SPECIAL);
				if ((h = mpool_get(t->bt_mp, pg, 0)) == NULL)
					return (RET_ERROR);
			} while (NEXTINDEX(h) == 0);
			index = NEXTINDEX(h) - 1;
		}
		break;
	}

	e->page = h;
	e->index = index;

	/*
	 * Delete any already deleted record that we've been saving because the
	 * cursor pointed to it.  This could cause the new index to be shifted
	 * down by one if the record we're deleting is on the same page and has
	 * a larger index.
	 */
	if (ISSET(t, BTF_DELCRSR)) {
		CLR(t, BTF_DELCRSR);			/* Don't try twice. */
		if (c->pgno == delc.pgno && c->index > delc.index)
			--c->index;
		if (__bt_crsrdel(t, &delc))
			return (RET_ERROR);
	}
	return (RET_SUCCESS);
}

/*
 * __BT_CRSRDEL -- Delete the record referenced by the cursor.
 *
 * Parameters:
 *	t:	tree
 *
 * Returns:
 *	RET_ERROR, RET_SUCCESS
 */
int
__bt_crsrdel(t, c)
	BTREE *t;
	EPGNO *c;
{
	PAGE *h;
	int status;

	CLR(t, BTF_DELCRSR);			/* Don't try twice. */
	if ((h = mpool_get(t->bt_mp, c->pgno, 0)) == NULL)
		return (RET_ERROR);
	status = __bt_dleaf(t, h, c->index);
	mpool_put(t->bt_mp, h, MPOOL_DIRTY);
	return (status);
}
