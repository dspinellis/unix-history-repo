/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)rec_utils.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/param.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <db.h>
#include "recno.h"

/*
 * __REC_RET -- Build return data as a result of search or scan.
 *
 * Parameters:
 *	t:	tree
 *	d:	LEAF to be returned to the user.
 *	data:	user's data structure
 *
 * Returns:
 *	RET_SUCCESS, RET_ERROR.
 */
int
__rec_ret(t, e, nrec, key, data)
	BTREE *t;
	EPG *e;
	recno_t nrec;
	DBT *key, *data;
{
	register RLEAF *rl;
	register void *p;

	if (data == NULL)
		goto retkey;

	rl = GETRLEAF(e->page, e->index);

	if (rl->flags & P_BIGDATA) {
		if (__ovfl_get(t, rl->bytes,
		    &data->size, &t->bt_dbuf, &t->bt_dbufsz))
			return (RET_ERROR);
	} else {
		/* Use +1 in case the first record retrieved is 0 length. */
		if (rl->dsize + 1 > t->bt_dbufsz) {
			if ((p = realloc(t->bt_dbuf, rl->dsize + 1)) == NULL)
				return (RET_ERROR);
			t->bt_dbuf = p;
			t->bt_dbufsz = rl->dsize + 1;
		}
		memmove(t->bt_dbuf, rl->bytes, rl->dsize);
		data->size = rl->dsize;
	}
	data->data = t->bt_dbuf;

retkey:	if (key == NULL)
		return (RET_SUCCESS);

	if (sizeof(recno_t) > t->bt_kbufsz) {
		if ((p = realloc(t->bt_kbuf, sizeof(recno_t))) == NULL)
			return (RET_ERROR);
		t->bt_kbuf = p;
		t->bt_kbufsz = sizeof(recno_t);
	}
	memmove(t->bt_kbuf, &nrec, sizeof(recno_t));
	key->size = sizeof(recno_t);
	key->data = t->bt_kbuf;
	return (RET_SUCCESS);
}
