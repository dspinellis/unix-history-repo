/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Mike Olson.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)bt_close.c	8.3 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/param.h>

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <db.h>
#include "btree.h"

static int bt_meta __P((BTREE *));

/*
 * BT_CLOSE -- Close a btree.
 *
 * Parameters:
 *	dbp:	pointer to access method
 *
 * Returns:
 *	RET_ERROR, RET_SUCCESS
 */
int
__bt_close(dbp)
	DB *dbp;
{
	BTREE *t;
	int fd;

	t = dbp->internal;

	/* Toss any page pinned across calls. */
	if (t->bt_pinned != NULL) {
		mpool_put(t->bt_mp, t->bt_pinned, 0);
		t->bt_pinned = NULL;
	}

	/*
	 * Delete any already deleted record that we've been saving
	 * because the cursor pointed to it.
	 */
	if (ISSET(t, B_DELCRSR) && __bt_crsrdel(t, &t->bt_bcursor))
		return (RET_ERROR);

	if (__bt_sync(dbp, 0) == RET_ERROR)
		return (RET_ERROR);

	if (mpool_close(t->bt_mp) == RET_ERROR)
		return (RET_ERROR);

	if (t->bt_stack)
		free(t->bt_stack);
	if (t->bt_kbuf)
		free(t->bt_kbuf);
	if (t->bt_dbuf)
		free(t->bt_dbuf);

	fd = t->bt_fd;
	free(t);
	free(dbp);
	return (close(fd) ? RET_ERROR : RET_SUCCESS);
}

/*
 * BT_SYNC -- sync the btree to disk.
 *
 * Parameters:
 *	dbp:	pointer to access method
 *
 * Returns:
 *	RET_SUCCESS, RET_ERROR.
 */
int
__bt_sync(dbp, flags)
	const DB *dbp;
	u_int flags;
{
	BTREE *t;
	int status;
	PAGE *h;
	void *p;

	t = dbp->internal;

	/* Toss any page pinned across calls. */
	if (t->bt_pinned != NULL) {
		mpool_put(t->bt_mp, t->bt_pinned, 0);
		t->bt_pinned = NULL;
	}

	/* Sync doesn't currently take any flags. */
	if (flags != 0) {
		errno = EINVAL;
		return (RET_ERROR);
	}

	if (ISSET(t, B_INMEM | B_RDONLY) || !ISSET(t, B_MODIFIED))
		return (RET_SUCCESS);

	if (ISSET(t, B_METADIRTY) && bt_meta(t) == RET_ERROR)
		return (RET_ERROR);

	/*
	 * Nastiness.  If the cursor has been marked for deletion, but not
	 * actually deleted, we have to make a copy of the page, delete the
	 * key/data item, sync the file, and then restore the original page
	 * contents.
	 */
	if (ISSET(t, B_DELCRSR)) {
		if ((p = (void *)malloc(t->bt_psize)) == NULL)
			return (RET_ERROR);
		if ((h = mpool_get(t->bt_mp, t->bt_bcursor.pgno, 0)) == NULL)
			return (RET_ERROR);
		memmove(p, h, t->bt_psize);
		if ((status =
		    __bt_dleaf(t, h, t->bt_bcursor.index)) == RET_ERROR)
			goto ecrsr;
		mpool_put(t->bt_mp, h, MPOOL_DIRTY);
	}
		
	if ((status = mpool_sync(t->bt_mp)) == RET_SUCCESS)
		CLR(t, B_MODIFIED);

ecrsr:	if (ISSET(t, B_DELCRSR)) {
		if ((h = mpool_get(t->bt_mp, t->bt_bcursor.pgno, 0)) == NULL)
			return (RET_ERROR);
		memmove(h, p, t->bt_psize);
		free(p);
		mpool_put(t->bt_mp, h, MPOOL_DIRTY);
	}
	return (status);
}

/*
 * BT_META -- write the tree meta data to disk.
 *
 * Parameters:
 *	t:	tree
 *
 * Returns:
 *	RET_ERROR, RET_SUCCESS
 */
static int
bt_meta(t)
	BTREE *t;
{
	BTMETA m;
	void *p;

	if ((p = mpool_get(t->bt_mp, P_META, 0)) == NULL)
		return (RET_ERROR);

	/* Fill in metadata. */
	m.m_magic = BTREEMAGIC;
	m.m_version = BTREEVERSION;
	m.m_psize = t->bt_psize;
	m.m_free = t->bt_free;
	m.m_nrecs = t->bt_nrecs;
	m.m_flags = t->bt_flags & SAVEMETA;

	memmove(p, &m, sizeof(BTMETA));
	mpool_put(t->bt_mp, p, MPOOL_DIRTY);
	return (RET_SUCCESS);
}
