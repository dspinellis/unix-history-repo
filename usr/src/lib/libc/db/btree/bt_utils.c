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
static char sccsid[] = "@(#)bt_utils.c	5.8 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/param.h>

#include <db.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "btree.h"

/*
 * __BT_RET -- Build return key/data pair as a result of search or scan.
 *
 * Parameters:
 *	t:	tree
 *	d:	LEAF to be returned to the user.
 *	key:	user's key structure (NULL if not to be filled in)
 *	data:	user's data structure
 *
 * Returns:
 *	RET_SUCCESS, RET_ERROR.
 */
int
__bt_ret(t, e, key, data)
	BTREE *t;
	EPG *e;
	DBT *key, *data;
{
	register BLEAF *bl;
	register void *p;

	bl = GETBLEAF(e->page, e->index);

	if (bl->flags & P_BIGDATA) {
		if (__ovfl_get(t, bl->bytes + bl->ksize,
		    &data->size, &t->bt_dbuf, &t->bt_dbufsz))
			return (RET_ERROR);
	} else {
		/* Use +1 in case the first record retrieved is 0 length. */
		if (bl->dsize + 1 > t->bt_dbufsz) {
			if ((p = realloc(t->bt_dbuf, bl->dsize + 1)) == NULL)
				return (RET_ERROR);
			t->bt_dbuf = p;
			t->bt_dbufsz = bl->dsize + 1;
		}
		bcopy(bl->bytes + bl->ksize, t->bt_dbuf, bl->dsize);
		data->size = bl->dsize;
	}
	data->data = t->bt_dbuf;

	if (key == NULL)
		return (RET_SUCCESS);

	if (bl->flags & P_BIGKEY) {
		if (__ovfl_get(t, bl->bytes,
		    &key->size, &t->bt_kbuf, &t->bt_kbufsz))
			return (RET_ERROR);
	} else {
		if (bl->ksize > t->bt_kbufsz) {
			if ((p = realloc(t->bt_kbuf, bl->ksize)) == NULL)
				return (RET_ERROR);
			t->bt_kbuf = p;
			t->bt_kbufsz = bl->ksize;
		}
		bcopy(bl->bytes, t->bt_kbuf, bl->ksize);
		key->size = bl->ksize;
	}
	key->data = t->bt_kbuf;
	return (RET_SUCCESS);
}

/*
 * __BT_CMP -- Compare a key to a given record.
 *
 * Parameters:
 *	t:	tree
 *	k1:	DBT pointer of first arg to comparison
 *	e:	pointer to EPG for comparison
 *
 * Returns:
 *	< 0 if k1 is < record
 *	= 0 if k1 is = record
 *	> 0 if k1 is > record
 */
int
__bt_cmp(t, k1, e)
	BTREE *t;
	const DBT *k1;
	EPG *e;
{
	BINTERNAL *bi;
	BLEAF *bl;
	DBT k2;
	PAGE *h;
	void *bigkey;

	/*
	 * The left-most key on internal pages, at any level of the tree, is
	 * guaranteed by the following code to be less than any user key.
	 * This saves us from having to update the leftmost key on an internal
	 * page when the user inserts a new key in the tree smaller than
	 * anything we've yet seen.
	 */
	h = e->page;
	if (e->index == 0 && h->prevpg == P_INVALID && !(h->flags & P_BLEAF))
		return (1);

	bigkey = NULL;
	if (h->flags & P_BLEAF) {
		bl = GETBLEAF(h, e->index);
		if (bl->flags & P_BIGKEY)
			bigkey = bl->bytes;
		else {
			k2.data = bl->bytes;
			k2.size = bl->ksize;
		}
	} else {
		bi = GETBINTERNAL(h, e->index);
		if (bi->flags & P_BIGKEY)
			bigkey = bi->bytes;
		else {
			k2.data = bi->bytes;
			k2.size = bi->ksize;
		}
	}

	if (bigkey) {
		if (__ovfl_get(t, bigkey,
		    &k2.size, &t->bt_dbuf, &t->bt_dbufsz))
			return (RET_ERROR);
		k2.data = t->bt_dbuf;
	}
	return((*t->bt_cmp)(k1, &k2));
}

/*
 * __BT_DEFCMP -- Default comparison routine.
 *
 * Parameters:
 *	a:	DBT #1
 *	b: 	DBT #2
 *
 * Returns:
 *	< 0 if a is < b
 *	= 0 if a is = b
 *	> 0 if a is > b
 */
int
__bt_defcmp(a, b)
	const DBT *a, *b;
{
	register u_char *p1, *p2;
	register int diff, len;

	len = MIN(a->size, b->size);
	for (p1 = a->data, p2 = b->data; len--; ++p1, ++p2)
		if (diff = *p1 - *p2)
			return(diff);
	return(a->size - b->size);
}

/*
 * __BT_DEFPFX -- Default prefix routine.
 *
 * Parameters:
 *	a:	DBT #1
 *	b: 	DBT #2
 *
 * Returns:
 *	Number of bytes needed to distinguish b from a.
 */
int
__bt_defpfx(a, b)
	const DBT *a, *b;
{
	register u_char *p1, *p2;
	register int len;
	int cnt;

	cnt = 1;
	len = MIN(a->size, b->size);
	for (p1 = a->data, p2 = b->data; len--; ++p1, ++p2, ++cnt)
		if (*p1 != *p2)
			return(cnt);

	/* a->size must be <= b->size, or they wouldn't be in this order. */
	return (a->size < b->size ? a->size + 1 : a->size);
}
