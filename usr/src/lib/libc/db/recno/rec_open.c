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
static char sccsid[] = "@(#)rec_open.c	5.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <limits.h>
#include <db.h>
#include <unistd.h>
#include <stdio.h>
#include <stddef.h>
#include "../btree/btree.h"

DB *
__rec_open(fname, flags, mode, openinfo)
	const char *fname;
	int flags, mode;
	const RECNOINFO *openinfo;
{
	BTREE *t;
	BTREEINFO btopeninfo;
	DB *dbp;
	PAGE *h;
	struct stat sb;
	int rfd;

	/* Open the user's file -- if this fails, we're done. */
	if ((rfd = open(fname, flags, mode)) < 0)
		return (NULL);

	/* Create a btree in memory (backed by disk). */
	if (openinfo) {
		btopeninfo.flags = 0;
		btopeninfo.cachesize = openinfo->cachesize;
		btopeninfo.psize = 0;
		btopeninfo.compare = NULL;
		btopeninfo.lorder = openinfo->lorder;
		dbp = __bt_open(NULL, O_RDWR, S_IRUSR | S_IWUSR, &btopeninfo);
	} else
		dbp = __bt_open(NULL, O_RDWR, S_IRUSR | S_IWUSR, NULL);
	if (dbp == NULL) {
		(void)close(rfd);
		return (NULL);
	}

	/*
	 * Some fields in the tree structure are recno specific.  Fill them
	 * in and make the btree structure look like a recno structure.
	 */
	t = dbp->internal;
	if (openinfo) {
		if (openinfo->flags & R_FIXEDLEN)
			t->bt_flags |= BTF_FIXEDLEN;

		t->bt_reclen = openinfo->reclen;
		if (t->bt_reclen == 0) {
			errno = EINVAL;
			goto err;
		}

		t->bt_bval = openinfo->bval;
	} else
		t->bt_bval = '\n';

	t->bt_flags = BTF_RECNO;

	/*
 	 * In 4.4BSD stat(2) returns true for ISSOCK on pipes.  Until then,
	 * this is fairly close.  Pipes are read-only.
 	 */
	if (lseek(rfd, 0L, SEEK_CUR) == -1 && errno == ESPIPE) {
		SET(t, BTF_RDONLY);
		if ((t->bt_rfp = fdopen(rfd, "r")) == NULL)
			goto err;
		t->bt_irec = ISSET(t, BTF_FIXEDLEN) ? __rec_fpipe : __rec_vpipe;
	} else {
		if (fstat(rfd, &sb))
			goto err;
		if (!(flags & (O_RDWR | O_WRONLY)))
			SET(t, BTF_RDONLY);
		if ((t->bt_smap = mmap(NULL, sb.st_size, PROT_READ, MAP_FILE,
		    rfd, (off_t)0)) == NULL)
			goto err;
		t->bt_emap = t->bt_smap + sb.st_size;
		t->bt_rfd = rfd;
		t->bt_irec = ISSET(t, BTF_FIXEDLEN) ? __rec_fmap : __rec_vmap;
	}

	/* Use the recno routines. */
	dbp->close = __rec_close;
	dbp->del = __rec_delete;
	dbp->get = __rec_get;
	dbp->put = __rec_put;
	dbp->seq = __rec_seq;
	dbp->sync = __rec_sync;

	/* If the root page was created, reset the flags. */
	if ((h = mpool_get(t->bt_mp, P_ROOT, 0)) == NULL)
		goto err;
	if ((h->flags & P_TYPE) == P_BLEAF) {
		h->flags = h->flags & ~P_TYPE | P_RLEAF;
		mpool_put(t->bt_mp, h, MPOOL_DIRTY);
	} else
		mpool_put(t->bt_mp, h, 0);

	if (openinfo && openinfo->flags & R_SNAPSHOT &&
	    t->bt_irec(t, MAX_REC_NUMBER) == RET_ERROR)
                goto err;
	return (dbp);

err:	__bt_close(dbp);
	(void)close(rfd);
	return (NULL);
}
