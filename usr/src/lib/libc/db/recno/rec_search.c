/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)rec_search.c	8.3 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>

#include <errno.h>
#include <stdio.h>

#include <db.h>
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
 * Returns:
 *	The EPG for matching record, if any, or the EPG for the location
 *	of the key, if it were inserted into the tree, is entered into
 *	the bt_cur field of the tree.  A pointer to the field is returned.
 */
EPG *
__rec_search(t, recno, op)
	BTREE *t;
	recno_t recno;
	enum SRCHOP op;
{
	register indx_t index;
	register PAGE *h;
	EPGNO *parent;
	RINTERNAL *r;
	pgno_t pg;
	indx_t top;
	recno_t total;
	int sverrno;

	BT_CLR(t);
	for (pg = P_ROOT, total = 0;;) {
		if ((h = mpool_get(t->bt_mp, pg, 0)) == NULL)
			goto err;
		if (h->flags & P_RLEAF) {
			t->bt_cur.page = h;
			t->bt_cur.index = recno - total;
			return (&t->bt_cur);
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
err:	sverrno = errno;
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
	errno = sverrno;
	return (NULL);
}
