/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)rec_search.c	5.6 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>

#include <db.h>
#include <errno.h>
#include <stdio.h>

#include "recno.h"

/*
 * __REC_SEARCH -- Search a btree for a key.
 *
 * Parameters:
 *	t:	tree to search
 *	recno:	key to find
 *	op: 	search operation
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
__rec_search(t, recno, op)
	BTREE *t;
	recno_t recno;
	enum SRCHOP op;
{
	static EPG e;
	register index_t index;
	register PAGE *h;
	EPGNO *parent;
	RINTERNAL *r;
	pgno_t pg;
	index_t top;
	recno_t total;
	int serrno;

	BT_CLR(t);
	for (pg = P_ROOT, total = 0;;) {
		if ((h = mpool_get(t->bt_mp, pg, 0)) == NULL)
			goto err;
		if (h->flags & P_RLEAF) {
			e.page = h;
			e.index = recno - total;
			return (&e);
		}
		for (index = 0, top = NEXTINDEX(h);;) {
			r = GETRINTERNAL(h, index);
			if (++index == top || total + r->nrecs > recno)
				break;
			total += r->nrecs;
		}

		if (__bt_push(t, pg, index - 1) == RET_ERROR)
			return (NULL);
		
		pg = r->pgno;
		switch (op) {
		case SDELETE:
			--GETRINTERNAL(h, (index - 1))->nrecs;
			mpool_put(t->bt_mp, h, MPOOL_DIRTY);
			break;
		case SINSERT:
			++GETRINTERNAL(h, (index - 1))->nrecs;
			mpool_put(t->bt_mp, h, MPOOL_DIRTY);
			break;
		case SEARCH:
			mpool_put(t->bt_mp, h, 0);
			break;
		}

	}
	/* Try and recover the tree. */
err:	serrno = errno;
	if (op != SEARCH)
		while  ((parent = BT_POP(t)) != NULL) {
			if ((h = mpool_get(t->bt_mp, parent->pgno, 0)) == NULL)
				break;
			if (op == SINSERT)
				--GETRINTERNAL(h, parent->index)->nrecs;
			else
				++GETRINTERNAL(h, parent->index)->nrecs;
                        mpool_put(t->bt_mp, h, MPOOL_DIRTY);
                }
	errno = serrno;
	return (NULL);
}
