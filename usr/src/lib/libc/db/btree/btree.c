/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Mike Olson.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)btree.c	5.9 (Berkeley) 5/7/91";
#endif /* LIBC_SCCS and not lint */

/*
 *  btree.c -- implementation of btree access method for 4.4BSD.
 *
 *	The design here is based on that of the btree access method used
 *	in the Postgres database system at UC Berkeley.  The implementation
 *	is wholly independent of the Postgres code.
 *
 *	This implementation supports btrees on disk (supply a filename) or
 *	in memory (don't).  Public interfaces defined here are:
 *
 *		btree_open()	-- wrapper; returns a filled DB struct for
 *				   a btree.
 *
 *		bt_open()	-- open a new or existing btree.
 *		bt_get()	-- fetch data from a tree by key.
 *		bt_seq()	-- do a sequential scan on a tree.
 *		bt_put()	-- add data to a tree by key.
 *		bt_delete()	-- remove data from a tree by key.
 *		bt_close()	-- close a btree.
 *		bt_sync()	-- force btree pages to disk (disk trees only).
 */

#include <sys/param.h>
#include <sys/stat.h>
#include <signal.h>
#include <errno.h>
#include <fcntl.h>
#include <db.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "btree.h"

BTREEINFO _DefaultBTInfo = {
	0,	/* flags */
	0,	/* cachesize */
	0,	/* psize */
	strcmp,	/* compare */
	0
};

/*
 *  BTREE_OPEN -- Wrapper routine to open a btree.
 *
 *	Creates and fills a DB struct, and calls the routine that actually
 *	opens the btree.
 *
 *	Parameters:
 *		f:  filename to open
 *		flags:  flag bits passed to open
 *		mode:  permission bits, used if O_CREAT specified
 *		b:  BTREEINFO pointer
 *
 *	Returns:
 *		Filled-in DBT on success; NULL on failure, with errno
 *		set as appropriate.
 *
 *	Side Effects:
 *		Allocates memory for the DB struct.
 */

DB *
btree_open(f, flags, mode, b)
	const char *f;
	int flags;
	int mode;
	const BTREEINFO *b;
{
	DB *db;
	BTREE t;

	if ((db = (DB *) malloc((unsigned) sizeof(DB))) == (DB *) NULL)
		return ((DB *) NULL);

	if ((t = bt_open(f, flags, mode, b)) == (BTREE) NULL) {
		(void) free ((char *) db);
		return ((DB *) NULL);
	}

	db->internal = (char *) t;
	db->close = bt_close;
	db->del = bt_delete;
	db->get = bt_get;
	db->put = bt_put;
	db->seq = bt_seq;
	db->sync = bt_sync;
	db->type = DB_BTREE;

	return (db);
}

/*
 *  BT_OPEN -- Open a btree.
 *
 *	This routine creates the correct kind (disk or in-memory) of
 *	btree and initializes it as required.
 *
 *	Parameters:
 *		f -- name of btree (NULL for in-memory btrees)
 *		flags -- flags passed to open()
 *		mode -- mode passed to open ()
 *		b -- BTREEINFO structure, describing btree
 *
 *	Returns:
 *		(Opaque) pointer to the btree.  On failure, returns NULL
 *		with errno set as appropriate.
 *
 *	Side Effects:
 *		Allocates memory, opens files.
 */

BTREE
bt_open(f, flags, mode, b)
	char *f;
	int flags;
	int mode;
	BTREEINFO *b;
{
	BTREE_P t;
	HTABLE ht;
	int nbytes;
	int fd;
	CURSOR *c;
	BTMETA m;
	struct stat buf;

	/* use the default info if none was provided */
	if (b == (BTREEINFO *) NULL)
		b = &_DefaultBTInfo;

	if ((t = (BTREE_P) malloc((unsigned) sizeof *t)) == (BTREE_P) NULL)
		return ((BTREE) NULL);

	if (b->compare)
		t->bt_compare = b->compare;
	else
		t->bt_compare = strcmp;

	t->bt_fname = f;
	t->bt_curpage = (BTHEADER *) NULL;
	t->bt_free = P_NONE;
	c = &(t->bt_cursor);
	c->c_pgno = P_NONE;
	c->c_index = 0;
	c->c_flags = (u_char) NULL;
	c->c_key = (char *) NULL;
	t->bt_stack = (BTSTACK *) NULL;
	t->bt_flags = 0;

	/*
	 *  If no file name was supplied, this is an in-memory btree.
	 *  Otherwise, it's a disk-based btree.
	 */
	if (f == (char *) NULL) {
		/* in memory */
		if ((t->bt_psize = b->psize) < MINPSIZE) {
			if (t->bt_psize != 0) {
				(void) free ((char *) t);
				errno = EINVAL;
				return ((BTREE) NULL);
			}
			t->bt_psize = getpagesize();
		}

		nbytes = HTSIZE * sizeof(HTBUCKET *);
		if ((ht = (HTABLE) malloc((unsigned) nbytes))
		    == (HTABLE) NULL) {
			(void) free((char *) t);
			return ((BTREE) NULL);
		}
		(void) bzero((char *) ht, nbytes);
		t->bt_s.bt_ht = ht;
		t->bt_npages = 0;
		t->bt_lorder = BYTE_ORDER;
		if (!(b->flags & R_DUP))
			t->bt_flags |= BTF_NODUPS;
	} else {
		/* on disk */
		if ((fd = open(f, O_RDONLY, 0)) < 0) {
			/* doesn't exist yet, be sure page is big enough */
			if ((t->bt_psize = b->psize) < sizeof(BTHEADER)
			    && b->psize != 0) {
				(void) free((char *) t);
				errno = EINVAL;
				return ((BTREE) NULL);
			}
			if (b->lorder == 0)
				b->lorder = BYTE_ORDER;

			if (b->lorder != BIG_ENDIAN
			    && b->lorder != LITTLE_ENDIAN) {
				(void) free((char *) t);
				errno = EINVAL;
				return ((BTREE) NULL);
			}
			t->bt_lorder = b->lorder;
			if (!(b->flags & R_DUP))
				t->bt_flags |= BTF_NODUPS;
		} else {
			/* exists, get page size from file */
			if (read(fd, &m, sizeof(m)) < sizeof(m)) {
				(void) close(fd);
				(void) free((char *) t);
				errno = EINVAL;
				return ((BTREE) NULL);
			}

			/* lorder always stored in host-independent format */
			NTOHL(m.m_lorder);
			if (m.m_lorder != BIG_ENDIAN
			    && m.m_lorder != LITTLE_ENDIAN) {
				(void) free((char *) t);
				errno = EINVAL;
				return ((BTREE) NULL);
			}
			t->bt_lorder = m.m_lorder;

			if (t->bt_lorder != BYTE_ORDER) {
				BLSWAP(m.m_magic);
				BLSWAP(m.m_version);
				BLSWAP(m.m_psize);
				BLSWAP(m.m_free);
				BLSWAP(m.m_flags);
			}

			if (m.m_magic != BTREEMAGIC
			    || m.m_version != BTREEVERSION
			    || m.m_psize < MINPSIZE) {
				(void) close(fd);
				(void) free((char *) t);
#ifndef EFTYPE
#define EFTYPE	-100
#endif
				errno = EFTYPE;
				return ((BTREE) NULL);
			}
			t->bt_psize = m.m_psize;
			t->bt_free = m.m_free;
			t->bt_flags |= (m.m_flags & BTF_NODUPS) | BTF_METAOK;
			(void) close(fd);
		}

		/* now open the file the way the user wanted it */
		if ((t->bt_s.bt_d.d_fd = open(f, flags, mode)) < 0) {
			(void) free ((char *) t);
			return ((BTREE) NULL);
		}

		/* access method files are always close-on-exec */
		if (fcntl(t->bt_s.bt_d.d_fd, F_SETFL, 1) == -1) {
			(void) close(t->bt_s.bt_d.d_fd);
			(void) free ((char *) t);
			return ((BTREE) NULL);
		}

		/* get number of pages, page size if necessary */
		(void) fstat(t->bt_s.bt_d.d_fd, &buf);
		if (t->bt_psize == 0)
			t->bt_psize = buf.st_blksize;
		t->bt_npages = (pgno_t) (buf.st_size / t->bt_psize);

		/* page zero is metadata, doesn't count */
		if (t->bt_npages > 0)
			--(t->bt_npages);

		if (b->cachesize == 0)
			b->cachesize = DEFCACHE;

		/* get an lru buffer cache, if the user asked for one */
		if ((b->cachesize / t->bt_psize) > 0) {
			BTDISK *d = &(t->bt_s.bt_d);

			d->d_cache = lruinit(d->d_fd,
					     (int) (b->cachesize / t->bt_psize),
					     (int) t->bt_psize,
					     (char *) t->bt_lorder,
					     _bt_pgin, _bt_pgout);

			if (d->d_cache == (char *) NULL) {
				(void) free((char *) t);
				return ((BTREE) NULL);
			}
		}
	}

	/* remember if tree was opened for write */
	if (((flags & O_WRONLY) == O_WRONLY)
	    || ((flags & O_RDWR) == O_RDWR))
		t->bt_flags |= BTF_ISWRITE;

	return ((BTREE) t);
}

/*
 *  BT_GET -- Get an entry from a btree.
 *
 *	Does a key lookup in the tree to find the specified key, and returns
 *	the key/data pair found.
 *
 *	Parameters:
 *		tree -- btree in which to do lookup
 *		key -- key to find
 *		data -- pointer to DBT in which to return data
 *		flag -- ignored
 *
 *	Returns:
 *		RET_SUCCESS, RET_ERROR, or RET_SPECIAL if the key is not
 *		found.  If key is not found, nothing is stored in the
 *		return DBT 'data'.
 *
 *	Side Effects:
 *		None.
 *
 *	Warnings:
 *		Return data is statically allocated, and will be overwritten
 *		at the next call.
 */

int
bt_get(dbp, key, data, flag)
	DB *dbp; 
	DBT *key, *data;
	u_long flag;
{
	BTREE_P t = (BTREE_P) (dbp->internal);
	BTHEADER *h;
	DATUM *d;
	BTITEM *item;

#ifdef lint
	flag = flag;
#endif /* lint */

	/* lookup */
	item = _bt_search(t, key);

	/* clear parent pointer stack */
	while (_bt_pop(t) != P_NONE)
		continue;

	if (item == (BTITEM *) NULL)
		return (RET_ERROR);

	h = (BTHEADER *) t->bt_curpage;
	data->size = 0;
	data->data = (u_char *) NULL;

	/* match? */
	if (VALIDITEM(t, item)
	    && _bt_cmp(t, key->data, item->bti_index) == 0) {
		d = (DATUM *) GETDATUM(h, item->bti_index);
		return (_bt_buildret(t, d, data, key));
	}

	/* nope */
	return (RET_SPECIAL);
}

/*
 *  BT_PUT -- Add an entry to a btree.
 *
 *	The specified (key, data) pair is added to the tree.  If the tree
 *	was created for unique keys only, then duplicates will not be
 *	entered.  If the requested key exists in the tree, it will be over-
 *	written unless the flags parameter is R_NOOVERWRITE, in which case
 *	the update will not be done.  If duplicate keys are permitted in the
 *	tree, duplicates will be inserted and will not overwrite existing
 *	keys.  Nodes are split as required.
 *
 *	Parameters:
 *		tree -- btree in which to put the new entry
 *		key -- key component to add
 *		data -- data corresponding to key
 *		flag -- R_NOOVERWRITE or zero.
 *
 *	Returns:
 *		RET_SUCCESS, RET_ERROR, or RET_SPECIAL if the
 *		NOOVERWRITE flag was set and the specified key exists
 *		in the database.
 *
 *	Side Effects:
 *		None.
 */

int
bt_put(dbp, key, data, flag)
	DB *dbp;
	DBT *key, *data;
	u_long flag;
{
	BTREE_P t;
	BTITEM *item;

	t = (BTREE_P)dbp->internal;

	/* look for this key in the tree */
	item = _bt_search(t, key);

	/*
	 *  If this tree was originally created without R_DUP, then duplicate
	 *  keys are not allowed.  We need to check this at insertion time.
	 */

	if (VALIDITEM(t, item) && _bt_cmp(t, key->data, item->bti_index) == 0) {
		if ((t->bt_flags & BTF_NODUPS) && flag == R_NOOVERWRITE) {
			if (_bt_delone(t, item->bti_index) == RET_ERROR) {
				while (_bt_pop(t) != P_NONE)
					continue;
				return (RET_ERROR);
			}
		}
	}

	return (_bt_insert(t, item, key, data, flag));
}

/*
 *  BT_DELETE -- delete a key from the tree.
 *
 *	Deletes all keys (and their associated data items) matching the
 *	supplied key from the tree.  If the flags entry is R_CURSOR, then
 *	the current item in the active scan is deleted.
 *
 *	Parameters:
 *		tree -- btree from which to delete key
 *		key -- key to delete
 *		flags -- R_CURSOR or zero
 *
 *	Returns:
 *		RET_SUCCESS, RET_ERROR, or RET_SPECIAL if the specified
 *		key was not in the tree.
 *
 *	Side Effects:
 *		None.
 */

int
bt_delete(dbp, key, flags)
	DB *dbp;
	DBT *key;
	u_long flags;
{
	BTREE_P t;
	BTHEADER *h;
	BTITEM *item;
	int ndel = 0;

	t = (BTREE_P)dbp->internal;

	if (flags == R_CURSOR)
		return (_bt_crsrdel(t));

	/* find the first matching key in the tree */
	item = _bt_first(t, key);
	h = t->bt_curpage;

	/* don't need the descent stack for deletes */
	while (_bt_pop(t) != P_NONE)
		continue;

	/* delete all matching keys */
	for (;;) {
		while (VALIDITEM(t, item)
		       && (_bt_cmp(t, key->data, item->bti_index) == 0)) {
			if (_bt_delone(t, item->bti_index) == RET_ERROR)
				return (RET_ERROR);
			ndel++;
		}

		if (VALIDITEM(t, item) || h->h_nextpg == P_NONE)
			break;

		/* next page, if necessary */
		do {
			if (_bt_getpage(t, h->h_nextpg) == RET_ERROR)
				return (RET_ERROR);
			h = t->bt_curpage;
		} while (NEXTINDEX(h) == 0 && h->h_nextpg != P_NONE);

		item->bti_pgno = h->h_pgno;
		item->bti_index = 0;

		if (!VALIDITEM(t, item)
		    || _bt_cmp(t, key->data, item->bti_index) != 0)
			break;
	}

	/* flush changes to disk */
	if (ISDISK(t)) {
		if (h->h_flags & F_DIRTY) {
			if (_bt_write(t, t->bt_curpage, NORELEASE) == RET_ERROR)
				return (RET_ERROR);
		}
	}

	if (ndel == 0)
		return (RET_SPECIAL);

	return (RET_SUCCESS);
}

/*
 *  BT_SYNC -- sync the btree to disk.
 *
 *	Parameters:
 *		tree -- btree to sync.
 *
 *	Returns:
 *		RET_SUCCESS, RET_ERROR.
 */

bt_sync(dbp)
	DB *dbp;
{
	BTREE_P t;
	BTHEADER *h;
	pgno_t pgno;

	t = (BTREE_P)dbp->internal;

	/* if this is an in-memory btree, syncing is a no-op */
	if (!ISDISK(t))
		return (RET_SUCCESS);

	h = (BTHEADER *) t->bt_curpage;
	h->h_flags &= ~F_DIRTY;

	if (ISCACHE(t)) {
		pgno = t->bt_curpage->h_pgno;
		if (_bt_write(t, h, RELEASE) == RET_ERROR)
			return(RET_ERROR);
		if (lrusync(t->bt_s.bt_d.d_cache) < RET_ERROR)
			return (RET_ERROR);
		if (_bt_getpage(t, pgno) == RET_ERROR)
			return (RET_ERROR);
	} else {
		if (_bt_write(t, h, NORELEASE) == RET_ERROR)
			return (RET_ERROR);
	}

	return (fsync(t->bt_s.bt_d.d_fd));
}

/*
 *  BT_SEQ -- Sequential scan interface.
 *
 *	This routine supports sequential scans on the btree.  If called with
 *	flags set to R_CURSOR, or if no seq scan has been initialized in the
 *	current tree, we initialize the scan.  Otherwise, we advance the scan
 *	and return the next item.
 *
 *	Scans can be either keyed or non-keyed.  Keyed scans basically have
 *	a starting point somewhere in the middle of the tree.  Non-keyed
 *	scans start at an endpoint.  Also, scans can be backward (descending
 *	order), forward (ascending order), or no movement (keep returning
 *	the same item).
 *
 *	Flags is checked every time we enter the routine, so the user can
 *	change directions on an active scan if desired.  The key argument
 *	is examined only when we initialize the scan, in order to position
 *	it properly.
 *
 *	Items are returned via the key and data arguments passed in.
 *
 *	Parameters:
 *		tree -- btree in which to do scan
 *		key -- key, used to position scan on initialization, and
 *		       used to return key components to the user.
 *		data -- used to return data components to the user.
 *		flags -- specify R_CURSOR, R_FIRST, R_LAST, R_NEXT, or
 *			 R_PREV.
 *
 *	Returns:
 *		RET_SUCCESS, RET_ERROR, or RET_SPECIAL if no more data
 *		exists in the tree in the specified direction.
 *
 *	Side Effects:
 *		Changes the btree's notion of the current position in the
 *		scan.
 *
 *	Warnings:
 *		The key and data items returned are static and will be
 *		overwritten by the next bt_get or bt_seq.
 */

int
bt_seq(dbp, key, data, flags)
	DB *dbp;
	DBT *key, *data;
	u_long flags;
{
	BTREE_P t;
	BTHEADER *h;
	DATUM *d;
	int status;

	t = (BTREE_P)dbp->internal;

	/* do we need to initialize the scan? */
	if (flags == R_CURSOR || flags == R_LAST || flags == R_FIRST
	    || !(t->bt_flags & BTF_SEQINIT)) {

		/* initialize it */
		status = _bt_seqinit(t, key, flags);
	} else {
		/* just advance the current scan pointer */
		status = _bt_seqadvance(t, flags);
	}

	key->size = data->size = 0;
	key->data = data->data = (u_char *) NULL;

	h = t->bt_curpage;

	/* is there a valid item at the current scan location? */
	if (status == RET_SPECIAL) {
		if (flags == R_NEXT) {
			if (t->bt_cursor.c_index >= NEXTINDEX(h)) {
				if (NEXTINDEX(h) > 0)
					t->bt_cursor.c_index = NEXTINDEX(h) - 1;
				else
					t->bt_cursor.c_index = 0;
			}
		} else {
			t->bt_cursor.c_index = 0;
		}
		return (RET_SPECIAL);
	} else if (status == RET_ERROR)
		return (RET_ERROR);

	/* okay, return the data */
	d = (DATUM *) GETDATUM(h, t->bt_cursor.c_index);

	return (_bt_buildret(t, d, data, key));
}

/*
 *  BT_CLOSE -- Close a btree
 *
 *	Parameters:
 *		tree -- tree to close
 *
 *	Returns:
 *		RET_SUCCESS, RET_ERROR.
 *
 *	Side Effects:
 *		Frees space occupied by the tree.
 */

int
bt_close(dbp)
	DB *dbp;
{
	struct HTBUCKET *b, *sb;
	BTREE_P t;
	BTHEADER *h;
	HTABLE ht;
	int fd, i;
	char *cache;

	t = (BTREE_P)dbp->internal;

	if (t->bt_cursor.c_key != (char *) NULL)
		(void) free(t->bt_cursor.c_key);

	if (!ISDISK(t)) {
		/* in-memory tree, release hash table memory */
		ht = t->bt_s.bt_ht;
		for (i = 0; i < HTSIZE; i++) {
			if ((b = ht[i]) == (struct HTBUCKET *) NULL)
				break;
			do {
				sb = b;
				(void) free((char *) b->ht_page);
				b = b->ht_next;
				(void) free((char *) sb);
			} while (b != (struct HTBUCKET *) NULL);
		}
		(void) free ((char *) ht);
		(void) free ((char *) t);
		return (RET_SUCCESS);
	}

	if ((t->bt_flags & BTF_ISWRITE) && !(t->bt_flags & BTF_METAOK)) {
		if (_bt_wrtmeta(t) == RET_ERROR)
			return (RET_ERROR);
	}

	if (t->bt_curpage != (BTHEADER *) NULL) {
		h = t->bt_curpage;
		if (h->h_flags & F_DIRTY) {
			if (_bt_write(t, h, RELEASE) == RET_ERROR)
				return (RET_ERROR);
		} else {
			if (_bt_release(t, h) == RET_ERROR)
				return (RET_ERROR);
		}

		/* flush and free the cache, if there is one */
		if (ISCACHE(t)) {
			cache = t->bt_s.bt_d.d_cache;
			if (lrusync(cache) == RET_ERROR)
				return (RET_ERROR);
			lrufree(cache);
		}
		(void) free ((char *) h);
	}

	fd = t->bt_s.bt_d.d_fd;
	(void) free ((char *) t);
	return (close(fd));
}
