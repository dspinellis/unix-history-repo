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
static char sccsid[] = "@(#)bt_search.c	5.2 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <db.h>
#include "btree.h"

/*
 *  _BT_FIRST -- Find the first item in the tree that matches the supplied
 *		 key.
 *
 *	This routine supports deletion.  When the user supplies a key to
 *	be deleted, we find the first one, and iteratively delete all the
 *	matching ones that follow it.
 *
 *	Parameters:
 *		t -- btree in which to find first occurrence
 *		key -- key to find
 *
 *	Returns:
 *		The BTITEM for the matching item.  If there's no match,
 *		this may point to the first item > than the supplied key,
 *		or off the end of the page.
 *
 *	Warnings:
 *		The BTITEM returned is in static space and will be overwritten
 *		by the next search of any kind in any btree.
 */

BTITEM *
_bt_first(t, key)
	BTREE_P t;
	DBT *key;
{
	BTHEADER *h;
	BTITEM *item;
	index_t next;
	int r;

	/* find any matching item */
	item = _bt_search(t, key);
	h = t->bt_curpage;
	next = NEXTINDEX(h);

	/* if we're off the end of the page, search failed and we're done */
	if (item->bti_index >= next)
		return (item);

	/* as long as we have an exact match, walk backwards */
	while ((r = _bt_cmp(t, key->data, item->bti_index)) == 0) {

		/* at start of page? */
		if (item->bti_index == 0) {

			/* if no prev page, we're done */
			if (h->h_prevpg == P_NONE)
				return (item);

			/* walk backward, skipping empty pages */
			do {
				if (_bt_getpage(t, h->h_prevpg) == RET_ERROR)
					return ((BTITEM *) NULL);
				h = t->bt_curpage;
			} while (NEXTINDEX(h) == 0 && h->h_prevpg != P_NONE);

			if (NEXTINDEX(h) != 0)
				item->bti_index = NEXTINDEX(h) - 1;
			else
				item->bti_index = 0;

			item->bti_pgno = h->h_pgno;
		} else {
			item->bti_index--;
		}
	}

	/* if we went too far backwards, step forward one entry */
	if (r > 0) {
		if (++(item->bti_index) >= NEXTINDEX(h)
		    && h->h_nextpg != P_NONE) {

			/* walk forward, skipping empty pages */
			do {
				if (_bt_getpage(t, h->h_nextpg) == RET_ERROR)
					return ((BTITEM *) NULL);
				h = t->bt_curpage;
			} while (h->h_nextpg != P_NONE && NEXTINDEX(h) == 0);

			item->bti_index = 0;
			item->bti_pgno = h->h_pgno;
		}
	}

	/* got it */
	return (item);
}

/*
 *  _BT_SEARCH, _BT_SEARCHR -- Search for a particular key in the tree.
 *
 *	Parameters:
 *		t -- btree in which to search
 *		key -- key to find
 *
 *	Returns:
 *		BTITEM for matching item, if any, or the BTITEM for the
 *		location of the key, if it were in the tree.
 *
 *	Warnings:
 *		The BTITEM returned is in static memory, and will be
 *		overwritten by the next search of any kind in any tree.
 */

BTITEM *
_bt_search(t, key)
	BTREE_P t;
	DBT *key;
{
	/* we want to start all of our searches at the root */
	if (_bt_getpage(t, (pgno_t) P_ROOT) == RET_ERROR)
		return ((BTITEM *) NULL);

	return (_bt_searchr(t, key));
}

BTITEM *
_bt_searchr(t, key)
	BTREE_P t;
	DBT *key;
{
	BTHEADER *h = t->bt_curpage;
	index_t index;
	IDATUM *id;
	DATUM *d;
	static BTITEM item;

	/* do a binary search on the current page */
	index = _bt_binsrch(t, key->data);

	/*
	 *  At this point, the binary search terminated because the endpoints
	 *  got too close together, or we have a match.  Figure out which
	 *  case applies and decide what to do based on the page type.
	 */
	if (h->h_flags & F_LEAF) {
		item.bti_pgno = h->h_pgno;
		item.bti_index = index;
		if (index < NEXTINDEX(h))
			d = (DATUM *) GETDATUM(h,index);
		else
			d = (DATUM *) NULL;

		item.bti_datum = d;
		return(&item);
	} else {
		id = (IDATUM *) GETDATUM(h, index);
		if (_bt_push(t, h->h_pgno) == RET_ERROR)
			return ((BTITEM *) NULL);
		if (_bt_getpage(t, id->i_pgno) == RET_ERROR)
			return ((BTITEM *) NULL);
		return (_bt_searchr(t, key));
	}
}

/*
 *  _BT_BINSRCH -- Do a binary search for a given key on the current page.
 *
 *	Searches on internal pages are handled slightly differently from
 *	searches on leaf pages.  This is because internal page searches
 *	find the largest item <= key in the tree, and leaf searches find
 *	the smallest item >= key.  This guarantees that leaf page searches
 *	leave us pointing at the item's correct position, and internal
 *	searches descend the tree correctly.
 *
 *	Parameters:
 *		t -- tree to search
 *		key -- key we're looking for
 *
 *	Returns:
 *		Index of the line pointer array entry for the (closest)
 *		match to key on the current page, with "closest" as defined
 *		above.
 */

index_t
_bt_binsrch(t, key)
	BTREE_P t;
	char *key;
{
	index_t lbound, ubound, cur;
	BTHEADER *h = t->bt_curpage;
	int match = 0;
	int r;

	lbound = 0;
	ubound = NEXTINDEX(h);
	if (ubound > 0)
		--ubound;

	/* do a binary search on the current page */
	while ((ubound - lbound) > 1) {
		cur = lbound + ((ubound - lbound) / 2);
		r = _bt_cmp(t, key, cur);

		if (r > 0)
			lbound = cur + 1;
		else if (r < 0)
			ubound = cur;
		else {
			match++;
			break;
		}
	}

	/*
	 *  At this point, the binary search terminated because the endpoints
	 *  got too close together, or we have a match.  Figure out which
	 *  case applies, decide what to do based on the page type (leaf or
	 *  internal), and do the right thing.
	 */
	if (match) {
		return (cur);
	} else if (ubound != lbound) {
		if (h->h_flags & F_LEAF) {
			r = _bt_cmp(t, key, lbound);
			if (r <= 0) {
				return (lbound);
			}
		} else {
			r = _bt_cmp(t, key, ubound);

			/* for internal nodes, move as far left as possible */
			if (r < 0) {
				r = _bt_cmp(t, key, lbound);
				if (r < 0 && lbound > 0)
					--lbound;
				return (lbound);
			} else {
				return (ubound);
			}
		}
	}

	if (h->h_flags & F_LEAF) {
		if (ubound < NEXTINDEX(h)) {
			r = _bt_cmp(t, key, ubound);
			if (r > 0)
				ubound++;
		}
	} else {
		/* for internal pages, move as far left as possible */
		if (ubound == NEXTINDEX(h))
			ubound--;

		while (_bt_cmp(t, key, ubound) < 0)
			ubound--;
	}
	return (ubound);
}
