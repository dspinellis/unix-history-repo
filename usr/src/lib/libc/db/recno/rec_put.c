/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)rec_put.c	5.7 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>

#include <db.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "recno.h"

/*
 * __REC_PUT -- Add a recno item to the tree.
 *
 * Parameters:
 *	dbp:	pointer to access method
 *	key:	key
 *	data:	data
 *	flag:	R_CURSORLOG, R_CURSOR, R_IAFTER, R_IBEFORE, R_NOOVERWRITE
 *
 * Returns:
 *	RET_ERROR, RET_SUCCESS and RET_SPECIAL if the key is already in the
 *	tree and R_NOOVERWRITE specified.
 */
int
__rec_put(dbp, key, data, flags)
	const DB *dbp;
	DBT *key;
	const DBT *data;
	u_int flags;
{
	BTREE *t;
	DBT tdata;
	recno_t nrec;
	int status;

	t = dbp->internal;

	switch (flags) {
	case R_CURSOR:
		if (!ISSET(t, BTF_SEQINIT))
			goto einval;
		nrec = t->bt_rcursor;
		break;
	case R_CURSORLOG:
		nrec = t->bt_rcursor + 1;
		SET(t, BTF_SEQINIT);
		break;
	case R_SETCURSOR:
		if ((nrec = *(recno_t *)key->data) == 0)
			goto einval;
		break;
	case R_IAFTER:
		if ((nrec = *(recno_t *)key->data) == 0) {
			nrec = 1;
			flags = R_IBEFORE;
		}
		break;
	case 0:
	case R_IBEFORE:
		if ((nrec = *(recno_t *)key->data) == 0)
			goto einval;
		break;
	case R_NOOVERWRITE:
		if ((nrec = *(recno_t *)key->data) == 0)
			goto einval;
		if (nrec <= t->bt_nrecs)
			return (RET_SPECIAL);
		break;
	default:
einval:		errno = EINVAL;
		return (RET_ERROR);
	}

	/*
	 * Make sure that records up to and including the put record are
	 * already in the database.  If skipping records, create empty ones.
	 */
	if (nrec > t->bt_nrecs) {
		if (!ISSET(t, BTF_RINMEM) && t->bt_irec(t, nrec) == RET_ERROR)
			return (RET_ERROR);
		if (nrec > t->bt_nrecs + 1) {
			tdata.data = NULL;
			tdata.size = 0;
			while (nrec > t->bt_nrecs + 1)
				if (__rec_iput(t,
				    t->bt_nrecs, &tdata, 0) != RET_SUCCESS)
					return (RET_ERROR);
		}
	}

	if ((status = __rec_iput(t, nrec - 1, data, flags)) != RET_SUCCESS)
		return (status);

	SET(t, BTF_MODIFIED);
	switch(flags) {
	case R_CURSORLOG:
		++t->bt_rcursor;
		break;
	case R_SETCURSOR:
		t->bt_rcursor = nrec;
		break;
	}
	
	return (__rec_ret(t, NULL, nrec, key, NULL));
}

/*
 * __REC_IPUT -- Add a recno item to the tree.
 *
 * Parameters:
 *	t:	tree
 *	nrec:	record number
 *	data:	data
 *
 * Returns:
 *	RET_ERROR, RET_SUCCESS
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
	PAGE *h;
	index_t index, nxtindex;
	pgno_t pg;
	size_t nbytes;
	int dflags, status;
	char *dest, db[NOVFLSIZE];

	/*
	 * If the data won't fit on a page, store it on indirect pages.
	 *
	 * XXX
	 * If the insert fails later on, these pages aren't recovered.
	 */
	if (data->size > t->bt_ovflsize) {
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
	if ((e = __rec_search(t, nrec,
	    nrec > t->bt_nrecs || flags == R_IAFTER || flags == R_IBEFORE ?
	    SINSERT : SEARCH)) == NULL)
		return (RET_ERROR);

	h = e->page;
	index = e->index;

	/*
	 * Add the specified key/data pair to the tree.  The R_IAFTER and
	 * R_IBEFORE flags insert the key after/before the specified key.
	 *
	 * Pages are split as required.
	 */
	switch (flags) {
	case R_IAFTER:
		++index;
		break;
	case R_IBEFORE:
		break;
	default:
		if (nrec < t->bt_nrecs &&
		    __rec_dleaf(t, h, index) == RET_ERROR) {
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
	if (h->upper - h->lower < nbytes + sizeof(index_t)) {
		status = __bt_split(t, h, NULL, data, dflags, nbytes, index);
		if (status == RET_SUCCESS)
			++t->bt_nrecs;
		return (status);
	}

	if (index < (nxtindex = NEXTINDEX(h)))
		bcopy(h->linp + index, h->linp + index + 1,
		    (nxtindex - index) * sizeof(index_t));
	h->lower += sizeof(index_t);

	h->linp[index] = h->upper -= nbytes;
	dest = (char *)h + h->upper;
	WR_RLEAF(dest, data, dflags);

	mpool_put(t->bt_mp, h, MPOOL_DIRTY);
	++t->bt_nrecs;
	return (RET_SUCCESS);
}
