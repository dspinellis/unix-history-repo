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
static char sccsid[] = "@(#)bt_delete.c	5.3 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <errno.h>
#include <db.h>
#include <stdio.h>
#include <string.h>
#include "btree.h"

static int bt_bdelete __P((BTREE *, const DBT *));

/*
 * __BT_DELETE -- Delete the item(s) referenced by a key.
 *
 * Parameters:
 *	dbp:	pointer to access method
 *	key:	key to delete
 *	flags:	R_CURSOR if deleting what the cursor references
 *
 * Returns:
 *	RET_ERROR, RET_SUCCESS and RET_SPECIAL if the key not found.
 */
int
__bt_delete(dbp, key, flags)
	const DB *dbp;
	const DBT *key;
	u_int flags;
{
	BTREE *t;
	int status;

	t = dbp->internal;
	if (ISSET(t, BTF_RDONLY)) {
		errno = EPERM;
		return (RET_ERROR);
	}
	switch(flags) {
	case 0:
		status = bt_bdelete(t, key);
		break;
	case R_CURSOR:
		/*
		 * If flags is R_CURSOR, delete the cursor; must already have
		 * started a scan and not have already deleted the record.  For
		 * the delete cursor bit to have been set requires that the
		 * scan be initialized, so no reason to check.
		 */
		status = ISSET(t, BTF_DELCRSR) ?
		    RET_SPECIAL : __bt_crsrdel(t, &t->bt_bcursor);
		break;
	default:
		errno = EINVAL;
		return (RET_ERROR);
	}
	if (status == RET_SUCCESS)
		SET(t, BTF_MODIFIED);
	return (status);
}

/*
 * BT_BDELETE -- Delete all key/data pairs matching the specified key.
 *
 * Parameters:
 *	tree:	tree
 *	key:	key to delete
 *
 * Returns:
 *	RET_ERROR, RET_SUCCESS and RET_SPECIAL if the key not found.
 */
static int
bt_bdelete(t, key)
	BTREE *t;
	const DBT *key;
{
	EPG *e, save;
	PAGE *h;
	pgno_t cpgno, pg;
	index_t cindex;
	int deleted, exact;

	/* Find any matching record; __bt_search pins the page. */
	if ((e = __bt_search(t, key, &exact)) == NULL)
		return (RET_ERROR);
	if (!exact) {
		mpool_put(t->bt_mp, e->page, 0);
		return (RET_SPECIAL);
	}

	/*
	 * Delete forward, then delete backward, from the found key.  The
	 * ordering is so that the deletions don't mess up the page refs.
	 * The first loop deletes the found key, the second unpins the found
	 * page.
	 *
	 * If find the key referenced by the cursor, don't delete it, just
	 * flag it for future deletion.  The cursor page number is P_INVALID
	 * unless the sequential scan is initialized, so no reason to check.
	 * A special case is when the already deleted cursor record was the
	 * only record found.  If so, then the delete opertion fails as no
	 * records were deleted.
	 *
	 * Cycle in place in the current page until the current record doesn't
	 * match the key or the page is empty.  If the latter, walk forward,
	 * skipping empty pages and repeating until an record doesn't match
	 * the key or the end of the tree is reached.
	 */
	cpgno = t->bt_bcursor.pgno;
	cindex = t->bt_bcursor.index;
	save = *e;
	for (h = e->page, deleted = 0;;) {
		do {
			if (h->pgno == cpgno && e->index == cindex) {
				if (NOTSET(t, BTF_DELCRSR)) {
					SET(t, BTF_DELCRSR);
					deleted = 1;
				}
				++e->index;
			} else {
				if (__bt_dleaf(t, h, e->index))
					goto err;
				mpool_put(t->bt_mp, h, MPOOL_DIRTY);
				deleted = 1;
			}
		} while (e->index < NEXTINDEX(h) && __bt_cmp(t, key, e) == 0);

		/*
		 * Quit if didn't find a match, no next page, or first key on
		 * the next page doesn't match.  Make a special effort not to
		 * unpin the page the original match was on, but also make sure
		 * it's unpinned if an error occurs.
		 */
		if (e->index < NEXTINDEX(h))
			break;
		for (;;) {
			if ((pg = h->nextpg) == P_INVALID)
				goto done1;
			if (h->pgno != save.page->pgno)
				mpool_put(t->bt_mp, h, 0);
			if ((h = mpool_get(t->bt_mp, pg, 0)) == NULL) {
				if (h->pgno == save.page->pgno)
					mpool_put(t->bt_mp, save.page, 0);
				return (RET_ERROR);
			}
			if (NEXTINDEX(h) != 0) {
				e->page = h;
				e->index = 0;
				break;
			}
		}

		if (__bt_cmp(t, key, e) != 0)
			break;
	}

	/*
	 * Reach here with the last page that was looked at pinned, and it may
	 * or may not be the same as the page with the original match.  If it's
	 * not, release it.
	 */
done1:	if (h->pgno != save.page->pgno)
		mpool_put(t->bt_mp, h, 0);

	/*
	 * Walk backwards from the record previous to the record returned by
	 * __bt_search, skipping empty pages, until a current record doesn't
	 * match the key or reach the beginning of the tree.
	 */
	*e = save;
	for (;;) {
		if (e->index)
			--e->index;
		for (h = e->page; e->index; --e->index) {
			if (__bt_cmp(t, key, e) != 0)
				goto done2;
			if (h->pgno == cpgno && e->index == cindex) {
				if (NOTSET(t, BTF_DELCRSR)) {
					SET(t, BTF_DELCRSR);
					deleted = 1;
				}
			} else {
				if (__bt_dleaf(t, h, e->index) == RET_ERROR)
					goto err;
				mpool_put(t->bt_mp, h, MPOOL_DIRTY);
				deleted = 1;
			}
		}

		if ((pg = h->prevpg) == P_INVALID)
			goto done2;
		mpool_put(t->bt_mp, h, 0);
		if ((e->page = mpool_get(t->bt_mp, pg, 0)) == NULL)
			return (RET_ERROR);
		e->index = NEXTINDEX(h);
	}

	/*
	 * Reach here with the last page that was looked at pinned.  Release
	 * it.
	 */
done2:	mpool_put(t->bt_mp, h, 0);
	return (deleted ? RET_SUCCESS : RET_SPECIAL);

err:	mpool_put(t->bt_mp, h, 0);
	return (RET_ERROR);
}

/*
 * __BT_DLEAF -- Delete a single record from a leaf page.
 *
 * Parameters:
 *	t:	tree
 *	index:	index on current page to delete
 *
 * Returns:
 *	RET_SUCCESS, RET_ERROR.
 */
int
__bt_dleaf(t, h, index)
	BTREE *t;
	PAGE *h;
	int index;
{
	register BLEAF *bl;
	register index_t *ip, offset;
	register size_t nbytes;
	register int cnt;
	char *from;
	void *to;

	/*
	 * Delete a record from a btree leaf page.  Internal records are never
	 * deleted from internal pages, regardless of the records that caused
	 * them to be added being deleted.  Pages made empty by deletion are
	 * not reclaimed.  They are, however, made available for reuse.
	 *
	 * Pack the remaining entries at the end of the page, shift the indices
	 * down, overwriting the deleted record and its index.  If the record
	 * uses overflow pages, make them available for reuse.
	 */
	to = bl = GETBLEAF(h, index);
	if (bl->flags & P_BIGKEY && __ovfl_delete(t, bl->bytes) == RET_ERROR)
		return (RET_ERROR);
	if (bl->flags & P_BIGDATA &&
	    __ovfl_delete(t, bl->bytes + bl->ksize) == RET_ERROR)
		return (RET_ERROR);
	nbytes = NBLEAF(bl);

	/*
	 * Compress the key/data pairs.  Compress and adjust the [BR]LEAF
	 * offsets.  Reset the headers.
	 */
	from = (char *)h + h->upper;
	bcopy(from, from + nbytes, (char *)to - from);
	h->upper += nbytes;

	offset = h->linp[index];
	for (cnt = &h->linp[index] - (ip = &h->linp[0]); cnt--; ++ip)
		if (ip[0] < offset)
			ip[0] += nbytes;
	for (cnt = &h->linp[NEXTINDEX(h)] - ip; --cnt; ++ip)
		ip[0] = ip[1] < offset ? ip[1] + nbytes : ip[1];
	h->lower -= sizeof(index_t);
	return (RET_SUCCESS);
}
