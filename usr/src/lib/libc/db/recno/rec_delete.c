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
static char sccsid[] = "@(#)rec_delete.c	5.4 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>

#include <db.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>

#include "recno.h"

static int rec_rdelete __P((BTREE *, recno_t));

/*
 * __REC_DELETE -- Delete the item(s) referenced by a key.
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
__rec_delete(dbp, key, flags)
	const DB *dbp;
	const DBT *key;
	u_int flags;
{
	BTREE *t;
	recno_t nrec;
	int status;

	t = dbp->internal;
	switch(flags) {
	case 0:
		if ((nrec = *(recno_t *)key->data) == 0)
			goto einval;
		if (nrec > t->bt_nrecs)
			return (RET_SPECIAL);
		--nrec;
		status = rec_rdelete(t, nrec);
		break;
	case R_CURSOR:
		if (!ISSET(t, BTF_SEQINIT))
			goto einval;
		if (t->bt_nrecs == 0)
			return (RET_SPECIAL);
		status = rec_rdelete(t, t->bt_rcursor - 1);
		if (status == RET_SUCCESS)
			--t->bt_rcursor;
		break;
	default:
einval:		errno = EINVAL;
		return (RET_ERROR);
	}

	if (status == RET_SUCCESS)
		SET(t, BTF_MODIFIED);
	return (status);
}

/*
 * REC_RDELETE -- Delete the data matching the specified key.
 *
 * Parameters:
 *	tree:	tree
 *	nrec:	record to delete
 *
 * Returns:
 *	RET_ERROR, RET_SUCCESS and RET_SPECIAL if the key not found.
 */
static int
rec_rdelete(t, nrec)
	BTREE *t;
	recno_t nrec;
{
	EPG *e;
	PAGE *h;
	int status;

	/* Find the record; __rec_search pins the page. */
	if ((e = __rec_search(t, nrec, SDELETE)) == NULL) {
		mpool_put(t->bt_mp, e->page, 0);
		return (RET_ERROR);
	}

	/* Delete the record. */
	h = e->page;
	status = __rec_dleaf(t, h, e->index);
	if (status != RET_SUCCESS) {
		mpool_put(t->bt_mp, h, 0);
		return (status);
	}
	mpool_put(t->bt_mp, h, MPOOL_DIRTY);
	return (RET_SUCCESS);
}

/*
 * __REC_DLEAF -- Delete a single record from a recno leaf page.
 *
 * Parameters:
 *	t:	tree
 *	index:	index on current page to delete
 *
 * Returns:
 *	RET_SUCCESS, RET_ERROR.
 */
int
__rec_dleaf(t, h, index)
	BTREE *t;
	PAGE *h;
	int index;
{
	register RLEAF *rl;
	register index_t *ip, offset;
	register size_t nbytes;
	register int cnt;
	char *from;
	void *to;

	/*
	 * Delete a record from a recno leaf page.  Internal records are never
	 * deleted from internal pages, regardless of the records that caused
	 * them to be added being deleted.  Pages made empty by deletion are
	 * not reclaimed.  They are, however, made available for reuse.
	 *
	 * Pack the remaining entries at the end of the page, shift the indices
	 * down, overwriting the deleted record and its index.  If the record
	 * uses overflow pages, make them available for reuse.
	 */
	to = rl = GETRLEAF(h, index);
	if (rl->flags & P_BIGDATA && __ovfl_delete(t, rl->bytes) == RET_ERROR)
		return (RET_ERROR);
	nbytes = NRLEAF(rl);

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
	--t->bt_nrecs;
	return (RET_SUCCESS);
}
