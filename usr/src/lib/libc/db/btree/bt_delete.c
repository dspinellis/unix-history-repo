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
static char sccsid[] = "@(#)bt_delete.c	5.2 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <db.h>
#include <errno.h>
#include <string.h>
#include "btree.h"

/*
 *  _BT_CRSRDEL -- Delete the item pointed to by the cursor.
 *
 *	This routine deletes the item most recently returned by a scan
 *	through the tree.  Since it only makes sense to delete the current
 *	record once, we make sure that we don't try to delete twice without
 *	advancing the scan.
 *
 *	Parameters:
 *		t -- tree in which to do deletion
 *
 *	Returns:
 *		RET_SUCCESS, RET_ERROR.
 *
 *	Side Effects:
 *		The call to _bt_delone marks the cursor, so we can tell that
 *		the current record has been deleted.
 */

int
_bt_crsrdel(t)
	BTREE_P t;
{
	CURSOR *c;

	c = &(t->bt_cursor);

	/* a cursor must exist, and can't have deleted the current key yet */
	if (!(t->bt_flags & BTF_SEQINIT) || (c->c_flags & CRSR_BEFORE)) {
		errno = EINVAL;
		return (RET_ERROR);
	}

	if (_bt_getpage(t, c->c_pgno) == RET_ERROR)
		return (RET_ERROR);

	if (c->c_index >= NEXTINDEX(t->bt_curpage)) {
		errno = EINVAL;
		return (RET_ERROR);
	}

	return (_bt_delone(t, c->c_index));
}

/*
 *  _BT_DELONE -- Delete a single entry from a btree.
 *
 *	This routine physically removes a btree entry from a leaf page.
 *	IDATUM items are *never* removed from internal nodes, regardless
 *	of whether the entries that originally caused them to be added
 *	are removed from the tree or not.  In addition, pages made empty
 *	by element deletion are not actually reclaimed.  They are,
 *	however, made available for reuse.
 *
 *	To delete an item from a page, we pack the remaining items at
 *	the end of the page, overwriting the deleted item's entry.  We
 *	move the line pointers backward on the page, overwriting the
 *	original item's line pointer.  This guarantees that the space in
 *	the middle of the page is free -- a property that our insertion
 *	strategy relies on.
 *
 *	This routine doesn't reclaim pages all of whose entries have
 *	been deleted.  These pages are available for reuse, however.
 *	If an item is deleted that was too big to fit on a page, then
 *	the blocks that it occupies are put on a free list for reuse.
 *
 *	Parameters:
 *		t -- btree from which to delete item
 *		index -- index of entry on current page to delete
 *
 *	Returns:
 *		RET_SUCCESS, RET_ERROR.
 *
 *	Side Effects:
 *		Physically changes page layout, adjusts internal page
 *		state to reflect the deletion of the item, and updates
 *		the list of free pages for this tree.
 */

int
_bt_delone(t, index)
	BTREE_P t;
	index_t index;
{
	char *src, *dest;
	int nbytes, nmoved;
	index_t off;
	index_t top;
	index_t i;
	pgno_t chain;
	BTHEADER *h;
	CURSOR *c;
	DATUM *d;

	/* deletion may confuse an active scan.  fix it.  */
	c = &(t->bt_cursor);
	if (t->bt_flags & BTF_SEQINIT && t->bt_curpage->h_pgno == c->c_pgno)
		if (_bt_fixscan(t, index, (DATUM *) NULL, DELETE) == RET_ERROR)
			return (RET_ERROR);

	h = t->bt_curpage;
	off = h->h_linp[index];
	d = (DATUM *) GETDATUM(h, index);

	/* if this is a big item, reclaim the space it occupies */
	if (d->d_flags & D_BIGKEY) {
		bcopy(&(d->d_bytes[0]),
		      (char *) &chain,
		      sizeof(chain));
		if (_bt_delindir(t, chain) == RET_ERROR)
			return (RET_ERROR);
		h = t->bt_curpage;
		d = (DATUM *) GETDATUM(h, index);
	}
	if (d->d_flags & D_BIGDATA) {
		bcopy(&(d->d_bytes[d->d_ksize]),
		      (char *) &chain,
		      sizeof(chain));
		if (_bt_delindir(t, chain) == RET_ERROR)
			return (RET_ERROR);
		h = t->bt_curpage;
		d = (DATUM *) GETDATUM(h, index);
	}

	/* move the data down on the page */
	nbytes = d->d_ksize + d->d_dsize
		 + (sizeof(DATUM) - sizeof(char));
	nbytes = LONGALIGN(nbytes);
	src = ((char *) h) + h->h_upper;
	dest = src + nbytes;
	nmoved = (int) (((char *) d) - src);
	(void) bcopy(src, dest, nmoved);

	/* next move the line pointers up */
	src = (char *) &(h->h_linp[index + 1]);
	dest = (char *) &(h->h_linp[index]);
	nmoved = (int) (((char *) &(h->h_linp[NEXTINDEX(h)])) - src);
	(void) bcopy(src, dest, nmoved);

	/* remember that we freed up some space */
	h->h_upper += nbytes;
	h->h_lower -= sizeof(index_t);

	/* adjust offsets in line pointers affected by moving the data */
	top = NEXTINDEX(h);
	for (i = 0; i < top; i++) {
		if (h->h_linp[i] < off)
			h->h_linp[i] += nbytes;
	}

	/* it's gone */
	h->h_flags |= F_DIRTY;

	return (RET_SUCCESS);
}
