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
static char sccsid[] = "@(#)bt_open.c	5.21 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

/*
 * Implementation of btree access method for 4.4BSD.
 *
 * The design here was originally based on that of the btree access method
 * used in the Postgres database system at UC Berkeley.  This implementation
 * is wholly independent of the Postgres code.
 */

#include <sys/param.h>
#include <sys/stat.h>

#include <signal.h>
#include <fcntl.h>
#include <errno.h>
#include <limits.h>
#define	__DBINTERFACE_PRIVATE
#include <db.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include "btree.h"

static int nroot __P((BTREE *));
static int tmp __P((void));

/*
 * __BT_OPEN -- Open a btree.
 *
 * Creates and fills a DB struct, and calls the routine that actually
 * opens the btree.
 *
 * Parameters:
 *	fname:	filename (NULL for in-memory trees)
 *	flags:	open flag bits
 *	mode:	open permission bits
 *	b:	BTREEINFO pointer
 *
 * Returns:
 *	NULL on failure, pointer to DB on success.
 *
 */
DB *
__bt_open(fname, flags, mode, openinfo)
	const char *fname;
	int flags, mode;
	const BTREEINFO *openinfo;
{
	BTMETA m;
	BTREE *t;
	BTREEINFO b;
	DB *dbp;
	pgno_t ncache;
	struct stat sb;
	int nr;

	/*
	 * Intention is to make sure all of the user's selections are okay
	 * here and then use them without checking.  Can't be complete, since
	 * we don't know the right page size, lorder or flags until the backing
	 * file is opened.  Also, the file's page size can cause the cachesize
	 * to change.
	 */
	if (openinfo) {
		b = *openinfo;

		/* Flags: R_DUP. */
		if (b.flags & ~(R_DUP))
			goto einval;

		/*
		 * Page size must be index_t aligned and >= MINPSIZE.  Default
		 * page size is set farther on, based on the underlying file
		 * transfer size.
		 */
		if (b.psize &&
		    (b.psize < MINPSIZE || b.psize > MAX_PAGE_OFFSET ||
		    b.psize & sizeof(index_t) - 1))
			goto einval;

		/* Minimum number of keys per page; absolute minimum is 2. */
		if (b.minkeypage) {
			if (b.minkeypage < 2)
				goto einval;
		} else
			b.minkeypage = DEFMINKEYPAGE;

		/* If no comparison, use default comparison and prefix. */
		if (b.compare == NULL) {
			b.compare = __bt_defcmp;
			if (b.prefix == NULL)
				b.prefix = __bt_defpfx;
		}

		if (b.lorder == 0)
			b.lorder = BYTE_ORDER;
		else if (b.lorder != BIG_ENDIAN && b.lorder != LITTLE_ENDIAN)
			goto einval;
	} else {
		b.compare = __bt_defcmp;
		b.cachesize = 0;
		b.flags = 0;
		b.lorder = BYTE_ORDER;
		b.minkeypage = DEFMINKEYPAGE;
		b.prefix = __bt_defpfx;
		b.psize = 0;
	}

	/* Allocate and initialize DB and BTREE structures. */
	if ((t = malloc(sizeof(BTREE))) == NULL)
		goto err;
	t->bt_fd = -1;			/* Don't close unopened fd on error. */
	if ((t->bt_dbp = dbp = malloc(sizeof(DB))) == NULL)
		goto err;
	t->bt_bcursor.pgno = P_INVALID;
	t->bt_bcursor.index = 0;
	t->bt_stack = NULL;
	t->bt_sp = t->bt_maxstack = 0;
	t->bt_kbuf = t->bt_dbuf = NULL;
	t->bt_kbufsz = t->bt_dbufsz = 0;
	t->bt_order = NOT;
	t->bt_cmp = b.compare;
	t->bt_pfx = b.prefix;
	t->bt_flags = 0;

	dbp->type = DB_BTREE;
	dbp->internal = t;
	dbp->close = __bt_close;
	dbp->del = __bt_delete;
	dbp->get = __bt_get;
	dbp->put = __bt_put;
	dbp->seq = __bt_seq;
	dbp->sync = __bt_sync;

	/*
	 * If no file name was supplied, this is an in-memory btree and we
	 * open a backing temporary file.  Otherwise, it's a disk-based tree.
	 */
	if (fname) {
		switch(flags & O_ACCMODE) {
		case O_RDONLY:
			SET(t, BTF_RDONLY);
			break;
		case O_RDWR:
			break;
		case O_WRONLY:
		default:
			goto einval;
		}
		
#define	USEFLAGS \
	(O_CREAT|O_EXCL|O_EXLOCK|O_RDONLY|O_RDWR|O_SHLOCK|O_TRUNC)
		if ((t->bt_fd = open(fname, flags & USEFLAGS, mode)) < 0)
			goto err;

	} else {
		if ((flags & O_ACCMODE) != O_RDWR)
			goto einval;
		if ((t->bt_fd = tmp()) == -1)
			goto err;
		SET(t, BTF_INMEM);
	}

	if (fcntl(t->bt_fd, F_SETFD, 1) == -1)
		goto err;

	if (fstat(t->bt_fd, &sb))
		goto err;
	if (sb.st_size) {
		nr = read(t->bt_fd, &m, sizeof(BTMETA));
		if (nr < 0)
			goto err;
		if (nr != sizeof(BTMETA))
			goto eftype;

		/*
		 * Read in the meta-data.  This can change the notion of what
		 * the lorder, page size and flags are, and, when the page size
		 * changes the cachesize value can change as well.
		 *
		 * Lorder is always stored in host-independent format.
		 */
		m.m_lorder = ntohl(m.m_lorder);
		if (m.m_lorder != BIG_ENDIAN && m.m_lorder != LITTLE_ENDIAN)
			goto eftype;
		if (m.m_lorder != BYTE_ORDER) {
			BLSWAP(m.m_magic);
			BLSWAP(m.m_version);
			BLSWAP(m.m_psize);
			BLSWAP(m.m_free);
			BLSWAP(m.m_nrecs);
			BLSWAP(m.m_flags);
		}
		if (m.m_magic != BTREEMAGIC || m.m_version != BTREEVERSION)
			goto eftype;
		if (m.m_psize < MINPSIZE || m.m_psize > MAX_PAGE_OFFSET ||
		    m.m_psize & sizeof(index_t) - 1)
			goto eftype;
		if (m.m_flags & ~SAVEMETA)
			goto eftype;
		b.psize = m.m_psize;
		t->bt_flags |= m.m_flags;
		t->bt_free = m.m_free;
		t->bt_lorder = m.m_lorder;
		t->bt_nrecs = m.m_nrecs;
	} else {
		/*
		 * Set the page size to the best value for I/O to this file.
		 * Don't overflow the page offset type.
		 */
		if (b.psize == 0) {
			b.psize = sb.st_blksize;
			if (b.psize < MINPSIZE)
				b.psize = MINPSIZE;
			if (b.psize > MAX_PAGE_OFFSET)
				b.psize = MAX_PAGE_OFFSET;
		}
		t->bt_flags |= b.flags & R_DUP ? 0 : BTF_NODUPS;
		t->bt_free = P_INVALID;
		t->bt_lorder = b.lorder;
		t->bt_nrecs = 0;
		SET(t, BTF_METADIRTY);
	}

	t->bt_psize = b.psize;

	/* Set the cache size; must be a multiple of the page size. */
	if (b.cachesize && b.cachesize & b.psize - 1)
		b.cachesize += (~b.cachesize & b.psize - 1) + 1;
	if (b.cachesize < b.psize * MINCACHE)
		b.cachesize = b.psize * MINCACHE;

	/* Calculate number of pages to cache. */
	ncache = (b.cachesize + t->bt_psize - 1) / t->bt_psize;

	/*
	 * The btree data structure requires that at least two keys can fit on
	 * a page, but other than that there's no fixed requirement.  The user
	 * specified a minimum number per page, and we translated that into the
	 * number of bytes a key/data pair can use before being placed on an
	 * overflow page.  This calculation includes the page header, the size
	 * of the index referencing the leaf item and the size of the leaf item
	 * structure.  Also, don't let the user specify a minkeypage such that
	 * a key/data pair won't fit even if both key and data are on overflow
	 * pages.
	 */
	t->bt_ovflsize = (t->bt_psize - BTDATAOFF) / b.minkeypage -
	    (sizeof(index_t) + NBLEAFDBT(0, 0));
	if (t->bt_ovflsize < NBLEAFDBT(NOVFLSIZE, NOVFLSIZE) + sizeof(index_t))
		t->bt_ovflsize =
		    NBLEAFDBT(NOVFLSIZE, NOVFLSIZE) + sizeof(index_t);

	/* Initialize the buffer pool. */
	if ((t->bt_mp =
	    mpool_open(NULL, t->bt_fd, t->bt_psize, ncache)) == NULL)
		goto err;
	if (NOTSET(t, BTF_INMEM))
		mpool_filter(t->bt_mp, __bt_pgin, __bt_pgout, t);

	/* Create a root page if new tree. */
	if (nroot(t) == RET_ERROR)
		goto err;

	return (dbp);

einval:	errno = EINVAL;
	goto err;

eftype:	errno = EFTYPE;
	goto err;

err:	if (t) {
		if (t->bt_dbp)
			free(t->bt_dbp);
		if (t->bt_fd != -1)
			(void)close(t->bt_fd);
		free(t);
	}
	return (NULL);
}

/*
 * NROOT -- Create the root of a new tree.
 *
 * Parameters:
 *	t:	tree
 *
 * Returns:
 *	RET_ERROR, RET_SUCCESS
 */
static int
nroot(t)
	BTREE *t;
{
	PAGE *meta, *root;
	pgno_t npg;

	if ((meta = mpool_get(t->bt_mp, 0, 0)) != NULL) {
		mpool_put(t->bt_mp, meta, 0);
		return (RET_SUCCESS);
	}
	if (errno != EINVAL)
		return (RET_ERROR);

	if ((meta = mpool_new(t->bt_mp, &npg)) == NULL)
		return (RET_ERROR);

	if ((root = mpool_new(t->bt_mp, &npg)) == NULL)
		return (RET_ERROR);

	if (npg != P_ROOT)
		return (RET_ERROR);
	root->pgno = npg;
	root->prevpg = root->nextpg = P_INVALID;
	root->lower = BTDATAOFF;
	root->upper = t->bt_psize;
	root->flags = P_BLEAF;
	bzero(meta, t->bt_psize);
	mpool_put(t->bt_mp, meta, MPOOL_DIRTY);
	mpool_put(t->bt_mp, root, MPOOL_DIRTY);
	return (RET_SUCCESS);
}

static int
tmp()
{
	sigset_t set, oset;
	int fd;
	char *envtmp;
	char path[MAXPATHLEN];

	envtmp = getenv("TMPDIR");
	(void)snprintf(path,
	    sizeof(path), "%s/bt.XXXXXX", envtmp ? envtmp : "/tmp");

	(void)sigfillset(&set);
	(void)sigprocmask(SIG_BLOCK, &set, &oset);
	if ((fd = mkstemp(path)) != -1)
		(void)unlink(path);
	(void)sigprocmask(SIG_SETMASK, &oset, NULL);
	return(fd);
}
