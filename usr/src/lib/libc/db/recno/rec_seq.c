/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)rec_seq.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <errno.h>
#include <db.h>
#include <limits.h>
#include <stdio.h>
#include "recno.h"

/*
 * __REC_SEQ -- Recno sequential scan interface.
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
__rec_seq(dbp, key, data, flags)
	const DB *dbp;
	DBT *key, *data;
	u_int flags;
{
	BTREE *t;
	EPG *e;
	recno_t nrec;
	int status;

	t = dbp->internal;
	switch(flags) {
	case R_CURSOR:
		if ((nrec = *(recno_t *)key->data) == 0) {
			errno = EINVAL;
			return (RET_ERROR);
		}
		break;
	case R_NEXT:
		if (ISSET(t, BTF_SEQINIT)) {
			nrec = t->bt_rcursor + 1;
			break;
		}
		/* FALLTHROUGH */
	case R_FIRST:
		nrec = 1;
		break;
	case R_PREV:
		if (ISSET(t, BTF_SEQINIT)) {
			if ((nrec = t->bt_rcursor - 1) == 0) {
				errno = EINVAL;
				return (RET_ERROR);
			}
			break;
		}
		/* FALLTHROUGH */
	case R_LAST:
		if (t->bt_irec(t, MAX_REC_NUMBER) == RET_ERROR)
			return (RET_ERROR);
		nrec = t->bt_nrecs;
		break;
	default:
		errno = EINVAL;
		return (RET_ERROR);
	}
	
	if (nrec > t->bt_nrecs) {
		if ((status = t->bt_irec(t, nrec)) != RET_SUCCESS)
			return (status);
		if (nrec > t->bt_nrecs)
			return (RET_SPECIAL);
	}

	if ((e = __rec_search(t, nrec - 1, SEARCH)) == NULL)
		return (RET_ERROR);

	if ((status = __rec_ret(t, e, data)) == RET_SUCCESS) {
		t->bt_rcursor = nrec;
		SET(t, BTF_SEQINIT);
		UNSET(t, BTF_DELCRSR);
	}
	mpool_put(t->bt_mp, e->page, 0);
	return (status);
}
