/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)rec_put.c	5.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <errno.h>
#include <db.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../btree/btree.h"

/*
 * __REC_PUT -- Add a recno item to the tree.
 *
 * Parameters:
 *	dbp:	pointer to access method
 *	key:	key
 *	data:	data
 *	flag:	R_NOOVERWRITE
 *
 * Returns:
 *	RET_ERROR, RET_SUCCESS and RET_SPECIAL if the key is already in the
 *	tree and R_NOOVERWRITE specified.
 */
int
__rec_put(dbp, key, data, flags)
	const DB *dbp;
	const DBT *key, *data;
	u_int flags;
{
	BTREE *t;
	DBT tdata;
	recno_t nrec;
	int status;

	if (flags &&
	    flags != R_IAFTER && flags != R_IBEFORE && flags != R_NOOVERWRITE ||
	    (nrec = *(recno_t *)key->data) == 0) {
		errno = EINVAL;
		return (RET_ERROR);
	}

	/*
	 * If skipping records, either get them from the original file or
	 * create empty ones.
	 */
	t = dbp->internal;
	if (nrec > t->bt_nrecs && t->bt_irec(t, nrec) == RET_ERROR)
		return (RET_ERROR);
	if (nrec > t->bt_nrecs) {
		tdata.data = NULL;
		tdata.size = 0;
		while (nrec > t->bt_nrecs) {
			status = __rec_iput(t, nrec, &tdata, 0);
			if (status != RET_SUCCESS)
				return (RET_ERROR);
		}
	}
	--nrec;
	if ((status = __rec_iput(t, nrec, data, flags)) == RET_SUCCESS)
		SET(t, BTF_MODIFIED);
	return (status);
}

/*
 * __REC_IPUT -- Add a recno item to the tree.
 *
 * Parameters:
 *	t:	tree
 *	nrec:	record number
 *	data:	data
 *	flag:	R_NOOVERWRITE
 *
 * Returns:
 *	RET_ERROR, RET_SUCCESS and RET_SPECIAL if the key is already in the
 *	tree and R_NOOVERWRITE specified.
 */
int
__rec_iput(t, nrec, data, flags)
	BTREE *t;
	recno_t nrec;
	const DBT *data;
	u_int flags;
{
	DBT tdata;
	EPG *e;
	EPGNO *parent;
	PAGE *h;
	index_t index, nxtindex;
	pgno_t pg;
	size_t nbytes;
	int dflags, exact;
	char *dest, db[NOVFLSIZE];

	/*
	 * If the data won't fit on a page, store it on indirect pages.
	 *
	 * XXX
	 * If the insert fails later on, these pages aren't recovered.
	 */
	if (data->size >= t->bt_minkeypage) {
		if (__ovfl_put(t, data, &pg) == RET_ERROR)
			return (RET_ERROR);
		tdata.data = db;
		tdata.size = NOVFLSIZE;
		*(pgno_t *)db = pg;
		*(size_t *)(db + sizeof(pgno_t)) = data->size;
		dflags = P_BIGDATA;
		data = &tdata;
	} else
		dflags = 0;

	/* __rec_search pins the returned page. */
	if ((e = __rec_search(t, nrec, &exact)) == NULL)
		return (RET_ERROR);

	h = e->page;
	index = e->index;

	/*
	 * Add the specified key/data pair to the tree.  If an identical key
	 * is already in the tree, and R_NOOVERWRITE is set, an error is
	 * returned.  If R_NOOVERWRITE is not set, the key is either added (if
	 * duplicates are permitted) or an error is returned.  The R_IAFTER
	 * and R_IBEFORE flags insert the key after/before the specified key.
	 *
	 * Pages are split as required.
	 */
	switch (flags) {
	case R_IAFTER:
		if (!exact) {
			errno = EINVAL;
			goto err;
		}
		++index;
		break;
	case R_IBEFORE:
		if (!exact) {
			errno = EINVAL;
			goto err;
		}
		break;
	case R_NOOVERWRITE:
		if (!exact)
			break;
		BT_CLR(t);
		mpool_put(t->bt_mp, h, 0);
		return (RET_SPECIAL);
	default:
		if (!exact || NOTSET(t, BTF_NODUPS))
			break;
		if (__rec_dleaf(t, h, index) == RET_ERROR) {
err:			BT_CLR(t);
			mpool_put(t->bt_mp, h, 0);
			return (RET_ERROR);
		}
		break;
	}

	/*
	 * If not enough room, split the page.  The split code will insert
	 * the key and data and unpin the current page.  If inserting into
	 * the offset array, shift the pointers up.
	 */
	nbytes = NRLEAFDBT(data->size);
	if (h->upper - h->lower < nbytes + sizeof(index_t))
		return (__bt_split(t, h, NULL, data, dflags, nbytes, index));

	if (index < (nxtindex = NEXTINDEX(h)))
		bcopy(h->linp + index, h->linp + index + 1,
		    (nxtindex - index) * sizeof(index_t));
	h->lower += sizeof(index_t);

	h->linp[index] = h->upper -= nbytes;
	dest = (char *)h + h->upper;
	WR_RLEAF(dest, data, dflags);

	mpool_put(t->bt_mp, h, MPOOL_DIRTY);

	/* Increment the count on all parent pages. */
	while  ((parent = BT_POP(t)) != NULL) {
		if ((h = mpool_get(t->bt_mp, parent->pgno, 0)) == NULL)
			return (RET_ERROR);
		++GETRINTERNAL(h, parent->index)->nrecs;
		mpool_put(t->bt_mp, h, MPOOL_DIRTY);
	}
	++t->bt_nrecs;
	return (RET_SUCCESS);
}
