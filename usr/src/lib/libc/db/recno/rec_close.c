/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)rec_close.c	5.2 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/param.h>
#include <sys/uio.h>
#include <errno.h>
#include <db.h>
#include <unistd.h>
#include <stdio.h>
#include "recno.h"

/*
 * __REC_CLOSE -- Close a recno tree.
 *
 * Parameters:
 *	dbp:	pointer to access method
 *
 * Returns:
 *	RET_ERROR, RET_SUCCESS
 */
int
__rec_close(dbp)
	DB *dbp;
{
	if (__rec_sync(dbp) == RET_ERROR)
		return (RET_ERROR);
	return (__bt_close(dbp));
}

/*
 * __REC_SYNC -- sync the recno tree to disk.
 *
 * Parameters:
 *	dbp:	pointer to access method
 *
 * Returns:
 *	RET_SUCCESS, RET_ERROR.
 *
 * XXX
 * Currently don't handle a key marked for deletion when the tree is synced.
 * Should copy the page and write it out instead of the real page.
 */
int
__rec_sync(dbp)
	const DB *dbp;
{
	struct iovec iov[2];
	BTREE *t;
	DBT data, key;
	off_t off;
	recno_t scursor;
	int status;

	t = dbp->internal;

	if (ISSET(t, BTF_INMEM) || NOTSET(t, BTF_MODIFIED))
		return (RET_SUCCESS);

	if (ISSET(t, BTF_RDONLY)) {
		errno = EPERM;
		return (RET_ERROR);
	}

	/* Suck any remaining records into the tree. */
	if (t->bt_irec(t, MAX_REC_NUMBER) == RET_ERROR)
		return (RET_ERROR);

	/* Rewind the file descriptor. */
	if (lseek(t->bt_rfd, 0L, SEEK_SET) != 0L)
		return (RET_ERROR);

	iov[1].iov_base = "\n";
	iov[1].iov_len = 1;
	scursor = t->bt_rcursor;

	status = (dbp->seq)(dbp, &key, &data, R_FIRST);
        while (status == RET_SUCCESS) {
		iov[0].iov_base = data.data;
		iov[0].iov_len = data.size;
		if (writev(t->bt_rfd, iov, 2) != data.size + 1)
			return (RET_ERROR);
                status = (dbp->seq)(dbp, &key, &data, R_NEXT);
        }
	t->bt_rcursor = scursor;
	if (status == RET_ERROR)
		return (RET_ERROR);
	if ((off = lseek(t->bt_rfd, 0L, SEEK_CUR)) == -1)
		return (RET_ERROR);
	if (ftruncate(t->bt_rfd, off))
		return (RET_ERROR);
	UNSET(t, BTF_MODIFIED);
	return (RET_SUCCESS);
}
