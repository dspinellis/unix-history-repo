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
static char sccsid[] = "@(#)storage.c	5.2 (Berkeley) 2/22/91";
#endif /* LIBC_SCCS and not lint */

#include <sys/param.h>
#include <db.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "btree.h"

/*
 *  BT_GETPAGE -- Make pgno the current page of the btree.
 *
 *	This routine is just a wrapper that decides whether to call the
 *	memory or disk-based routine to do the work.
 *
 *	Parameters:
 *		t -- btree in which to get page
 *		pgno -- page number to get
 *
 *	Returns:
 *		RET_SUCCESS or RET_ERROR.
 */

int
_bt_getpage(t, pgno)
	BTREE_P t;
	pgno_t pgno;
{
#ifdef DEBUG
	if (pgno == P_NONE)
		_punt();
#endif /* DEBUG */

	/* see if we can get away without doing any work */
	if (t->bt_curpage != (BTHEADER *) NULL) {
		if (t->bt_curpage->h_pgno == pgno)
			return (RET_SUCCESS);
	}

	if (t->bt_fname == (char *) NULL)
		return (_bt_getmpage(t, pgno));
	else
		return (_bt_getdpage(t, pgno));
}

/*
 *  _BT_GETMPAGE -- Make pgno the current page of the btree.
 *
 *	This routine gets pages for in-memory btrees.
 *
 *	Parameters:
 *		t -- btree in which to get page
 *		pgno -- page number to get
 *
 *	Returns:
 *		RET_SUCCESS or RET_ERROR.
 */

int
_bt_getmpage(t, pgno)
	register BTREE_P t;
	pgno_t pgno;
{
	int htindex;
	BTHEADER *h;
	HTBUCKET *b;

	if (t->bt_curpage == (BTHEADER *) NULL) {
		if (pgno != P_ROOT) {
			errno = EBADF;
			return (RET_ERROR);
		}

		t->bt_npages++;
		h = (BTHEADER *) malloc((unsigned) t->bt_psize);
		if (h == (BTHEADER *) NULL)
			return (RET_ERROR);

		h->h_pgno = P_ROOT;
		h->h_flags = F_LEAF;
		h->h_lower = (index_t)
				(((char *) &(h->h_linp[0])) - ((char *) h));
		h->h_upper = t->bt_psize;
		h->h_prevpg = h->h_nextpg = P_NONE;

		t->bt_curpage = h;

		/* get the root page into the hash table */
		if (_bt_write(t, h, RELEASE) == RET_ERROR)
			return (RET_ERROR);
	}

	htindex = HASHKEY(pgno);

	for (b = t->bt_s.bt_ht[htindex];
	     b != (HTBUCKET *) NULL;
	     b = b->ht_next) {
		if (b->ht_pgno == pgno) {
			t->bt_curpage = b->ht_page;
			return (RET_SUCCESS);
		}
	}
	return (RET_ERROR);
}

/*
 *  _BT_GETDPAGE -- Make pgno the current page of the btree.
 *
 *	This routine gets pages for disk btrees.
 *
 *	Because disk btree pages must be readable across machine architectures,
 *	the btree code writes integers out in network format.  This routine
 *	converts them back to host format before returning the page.
 *
 *	Parameters:
 *		t -- btree in which to get page
 *		pgno -- page number to get
 *
 *	Returns:
 *		RET_SUCCESS, RET_ERROR.
 */

int
_bt_getdpage(t, pgno)
	register BTREE_P t;
	pgno_t pgno;
{
	BTHEADER *h;
	char *cache;
	long pos;
	int n, nbytes;

	/* if we have an lru cache, let the cache code do the work */
	if (ISCACHE(t)) {
		cache = t->bt_s.bt_d.d_cache;

		/* release the old page */
		if (t->bt_curpage != (BTHEADER *) NULL) {
			pgno_t opgno = t->bt_curpage->h_pgno;
			t->bt_curpage->h_flags &= ~F_DIRTY;

			if (lruwrite(cache, (int) opgno) < 0)
				return (RET_ERROR);

			if (lrurelease(cache, (int) opgno) < 0)
				return (RET_ERROR);
		}

		if (pgno > t->bt_npages) {
			if ((h = (BTHEADER *) lrugetnew(cache, (int)pgno, &nbytes))
			    == (BTHEADER *) NULL)
				return (RET_ERROR);
			t->bt_npages = pgno;
		} else {
			if ((h = (BTHEADER *) lruget(cache, (int)pgno, &nbytes))
			    == (BTHEADER *) NULL)
				return (RET_ERROR);
		}

		/* init this page, if necessary */
		if (nbytes == 0) {
			h->h_pgno = pgno;
			h->h_flags = F_LEAF;
			h->h_lower = (index_t)
				(((char *) &(h->h_linp[0])) - ((char *) h));
			h->h_upper = t->bt_psize;
			h->h_prevpg = h->h_nextpg = P_NONE;
		}

		t->bt_curpage = h;

		return (RET_SUCCESS);
	}

	/* sync the current page, if necessary */
	if (t->bt_curpage != (BTHEADER *) NULL) {
		if (t->bt_curpage->h_flags & F_DIRTY)
			if (_bt_write(t, t->bt_curpage, RELEASE) == RET_ERROR)
				return (RET_ERROR);
	} else {
		if (t->bt_npages == 0)
			t->bt_npages = 1;

		/* if no current page, get space for one */
		if ((t->bt_curpage = (BTHEADER *) malloc((unsigned) t->bt_psize))
		    == (BTHEADER *) NULL) {
			return (RET_ERROR);
		}
	}

	n = t->bt_psize;
	pos = (long) (pgno * n);

	/* seek to correct location in file */
	if (lseek(t->bt_s.bt_d.d_fd, pos, L_SET) != pos) {
		return (RET_ERROR);
	}

	/* read the page */
	if ((nbytes = read(t->bt_s.bt_d.d_fd, t->bt_curpage, n)) < n) {

		/*
		 *  If we didn't get a full page, we must have gotten no
		 *  data at all -- in which case we're trying to read a
		 *  root page that doesn't exist yet.  This is the only
		 *  case in which this is okay.  If this happens, construct
		 *  an empty root page by hand.
		 */
		if (nbytes != 0 || pgno != P_ROOT) {
			errno = EBADF;
			return (RET_ERROR);
		}

		h = (BTHEADER *) t->bt_curpage;
		h->h_pgno = pgno;
		h->h_flags = F_LEAF;
		h->h_lower = (index_t)
				(((char *) &(h->h_linp[0])) - ((char *) h));
		h->h_upper = t->bt_psize;
		h->h_prevpg = h->h_nextpg = P_NONE;
	} else
		(void) _bt_pgin(t->bt_curpage, (char *) t->bt_lorder);

	return (RET_SUCCESS);
}

/*
 *  _BT_PGOUT, _BT_PGIN -- Convert host-specific number layout to/from
 *			   the host-independent format stored on disk.
 *
 *	Parameters:
 *		h -- page to convert
 *		_lorder -- byte order for pages (stored as a char * in the
 *			   cache, and passed around as a magic cookie).
 *
 *	Returns:
 *		RET_SUCCESS (lru code requires a return value).
 *
 *	Side Effects:
 *		Layout of tree metadata on the page is changed in place.
 *
 *	Warnings:
 *		Everywhere else in the code, the types pgno_t and index_t
 *		are opaque.  These two routines know what they really
 *		are.
 */

int
_bt_pgout(h, _lorder)
	BTHEADER *h;
	char *_lorder;
{
	int i;
	int top;
	int lorder;
	DATUM *d;
	IDATUM *id;
	size_t chain;

	lorder = (int) _lorder;
	if (lorder == BYTE_ORDER)
		return (RET_SUCCESS);

	if (h->h_flags & F_LEAF) {
		if (h->h_flags & F_CONT) {
			if (h->h_prevpg == P_NONE) {
				size_t longsz;

				(void) bcopy((char *) &(h->h_linp[0]),
					      (char *) &longsz,
					      sizeof(longsz));
				BLSWAP(longsz);
				(void) bcopy((char *) &longsz,
					      (char *) &(h->h_linp[0]),
					      sizeof(longsz));
			}
		} else {
			top = NEXTINDEX(h);
			for (i = 0; i < top; i++) {
				d = (DATUM *) GETDATUM(h, i);
				if (d->d_flags & D_BIGKEY) {
					(void) bcopy((char *) &(d->d_bytes[0]),
						      (char *) &chain,
						      sizeof(chain));
					BLSWAP(chain);
					(void) bcopy((char *) &chain,
						      (char *) &(d->d_bytes[0]),
						      sizeof(chain));
				}
				if (d->d_flags & D_BIGDATA) {
					(void) bcopy((char *) &(d->d_bytes[d->d_ksize]),
						      (char *) &chain,
						      sizeof(chain));
					BLSWAP(chain);
					(void) bcopy((char *) &chain,
						      (char *) &(d->d_bytes[d->d_ksize]),
						      sizeof(chain));
				}
				BLSWAP(d->d_dsize);
				BLSWAP(d->d_ksize);
				BLSWAP(d->d_flags);
				BLSWAP(h->h_linp[i]);
			}
		}
	} else {
		top = NEXTINDEX(h);
		for (i = 0; i < top; i++) {
			id = (IDATUM *) GETDATUM(h, i);
			BLSWAP(id->i_size);
			BLSWAP(id->i_pgno);
			BLSWAP(id->i_flags);
			if (id->i_flags & D_BIGKEY) {
				(void) bcopy((char *) &(id->i_bytes[0]),
					      (char *) &chain,
					      sizeof(chain));
				BLSWAP(chain);
				(void) bcopy((char *) &chain,
					      (char *) &(id->i_bytes[0]),
					      sizeof(chain));
			}
			BLSWAP(h->h_linp[i]);
		}
	}
	BLSWAP(h->h_flags);
	BLSWAP(h->h_pgno);
	BLSWAP(h->h_prevpg);
	BLSWAP(h->h_nextpg);
	BLSWAP(h->h_lower);
	BLSWAP(h->h_upper);

	return (RET_SUCCESS);
}

int
_bt_pgin(h, _lorder)
	BTHEADER *h;
	char *_lorder;
{
	int i;
	int top;
	int lorder;
	DATUM *d;
	IDATUM *id;
	size_t chain;

	/*
	 *  If btree pages are to be stored in the host byte order, don't
	 *  bother swapping.
	 */
	lorder = (int) _lorder;
	if (lorder == BYTE_ORDER)
		return (RET_SUCCESS);

	BLSWAP(h->h_upper);
	BLSWAP(h->h_lower);
	BLSWAP(h->h_nextpg);
	BLSWAP(h->h_prevpg);
	BLSWAP(h->h_pgno);
	BLSWAP(h->h_flags);

	if (h->h_flags & F_LEAF) {
		if (h->h_flags & F_CONT) {
			if (h->h_prevpg == P_NONE) {
				size_t longsz;

				(void) bcopy((char *) &(h->h_linp[0]),
					      (char *) &longsz,
					      sizeof(longsz));
				BLSWAP(longsz);
				(void) bcopy((char *) &longsz,
					      (char *) &(h->h_linp[0]),
					      sizeof(longsz));
			}
		} else {
			top = NEXTINDEX(h);
			for (i = 0; i < top; i++) {
				BLSWAP(h->h_linp[i]);
				d = (DATUM *) GETDATUM(h, i);
				BLSWAP(d->d_dsize);
				BLSWAP(d->d_ksize);
				BLSWAP(d->d_flags);
				if (d->d_flags & D_BIGKEY) {
					(void) bcopy((char *) &(d->d_bytes[0]),
						      (char *) &chain,
						      sizeof(chain));
					BLSWAP(chain);
					(void) bcopy((char *) &chain,
						      (char *) &(d->d_bytes[0]),
						      sizeof(chain));
				}
				if (d->d_flags & D_BIGDATA) {
					(void) bcopy((char *) &(d->d_bytes[d->d_ksize]),
						      (char *) &chain,
						      sizeof(chain));
					BLSWAP(chain);
					(void) bcopy((char *) &chain,
						      (char *) &(d->d_bytes[d->d_ksize]),
						      sizeof(chain));
				}
			}
		}
	} else {
		top = NEXTINDEX(h);
		for (i = 0; i < top; i++) {
			BLSWAP(h->h_linp[i]);
			id = (IDATUM *) GETDATUM(h, i);
			BLSWAP(id->i_size);
			BLSWAP(id->i_pgno);
			BLSWAP(id->i_flags);
			if (id->i_flags & D_BIGKEY) {
				(void) bcopy((char *) &(id->i_bytes[0]),
					      (char *) &chain,
					      sizeof(chain));
				BLSWAP(chain);
				(void) bcopy((char *) &chain,
					      (char *) &(id->i_bytes[0]),
					      sizeof(chain));
			}
		}
	}
	return (RET_SUCCESS);
}

/*
 *  _BT_ALLOCPG -- allocate a new page in the btree.
 *
 *	This is called when we split a page, to get space to do the split.
 *	For disk btrees, these pages are released when the split is done.
 *	For memory btrees, they are not.
 *
 *	Parameters:
 *		t -- tree in which to do split
 *
 *	Returns:
 *		Pointer to the newly-allocated page
 */

BTHEADER *
_bt_allocpg(t)
	BTREE_P t;
{
	BTHEADER *h = t->bt_curpage;
	BTHEADER *nh;
	int nbytes;

	/* if we have a cache, let the cache code do the work */
	if (ISDISK(t) && ISCACHE(t)) {
		nh = (BTHEADER *) lrugetnew(t->bt_s.bt_d.d_cache,
					    (int) (t->bt_npages + 1),
					    &nbytes);
	} else {
		nh = (BTHEADER *) malloc((unsigned) t->bt_psize);
	}

	if (nh != (BTHEADER *) NULL) {
		nh->h_pgno = nh->h_prevpg = nh->h_nextpg = P_NONE;
		nh->h_flags = h->h_flags;
		nh->h_lower = (index_t)
				(((char *) &(nh->h_linp[0])) - ((char *) nh));
		nh->h_upper = t->bt_psize;
	}

	return (nh);
}

/*
 *  _BT_WRITE -- Write a specific page to a btree file.
 *
 *	Parameters:
 *		t -- btree to get the page
 *		h -- page to write
 *		relflag -- (int) this page may/may not be released
 *
 *	Returns:
 *		RET_SUCCESS, RET_ERROR.
 *
 *	Side Effects:
 *		Writes a metadata page if none has been written yet.
 */

int
_bt_write(t, h, relflag)
	BTREE_P t;
	BTHEADER *h;
	int relflag;
{
	long pos;
	int htindex;
	HTBUCKET *b;
	char *cache;
	pgno_t pgno;

	h->h_flags &= ~F_DIRTY;
	if (ISDISK(t)) {

		/* if we haven't done so yet, write the metadata */
		if (!(t->bt_flags & BTF_METAOK)) {
			if (_bt_wrtmeta(t) == RET_ERROR)
				return (RET_ERROR);
		}

		pgno = h->h_pgno;


		/* if we have a cache, let the cache code do the work */
		if ((cache = t->bt_s.bt_d.d_cache) != (char *) NULL) {
			if (lruwrite(cache, (int) pgno) < 0)
				return (RET_ERROR);
			if (relflag && lrurelease(cache, (int) pgno) < 0)
				return (RET_ERROR);
				
		} else {
			(void) _bt_pgout(h, (char *) t->bt_lorder);
			/* now write the current page */
			pos = (long) (pgno * t->bt_psize);
			if (lseek(t->bt_s.bt_d.d_fd, pos, L_SET) != pos)
				return (RET_ERROR);
			if (write(t->bt_s.bt_d.d_fd, (char *) h, (int) t->bt_psize)
			    < t->bt_psize)
				return (RET_ERROR);
			if (!relflag)
				(void) _bt_pgin(h, (char *) t->bt_lorder);
		}
	} else {
		/* in-memory btree */
		htindex = HASHKEY(h->h_pgno);

		/* see if we need to overwrite existing entry */
		for (b = t->bt_s.bt_ht[htindex];
		     b != (HTBUCKET *) NULL;
		     b = b->ht_next) {
			if (b->ht_pgno == h->h_pgno) {
				b->ht_page = h;
				return (RET_SUCCESS);
			}
		}

		/* new entry, write it */
		b = (HTBUCKET *) malloc((unsigned) sizeof (HTBUCKET));
		if (b == (HTBUCKET *) NULL)
			return (RET_ERROR);

		b->ht_pgno = h->h_pgno;
		b->ht_page = h;
		b->ht_next = t->bt_s.bt_ht[htindex];
		t->bt_s.bt_ht[htindex] = b;
	}
	return (RET_SUCCESS);
}

/*
 *  _BT_RELEASE -- Release a locked-down cache page
 *
 *	During page splits, we want to force pages out to the cache
 *	before we actually release them, in some cases.  This routine
 *	releases such a page when it is no longer needed.
 *
 *	Parameters:
 *		t -- btree in which to release page
 *		h -- page to release
 *
 *	Returns:
 *		RET_SUCCESS, RET_ERROR.
 *
 *	Side Effects:
 *		None.
 */

int
_bt_release(t, h)
	BTREE_P t;
	BTHEADER *h;
{
	if (ISDISK(t) && ISCACHE(t)) {
		if (lrurelease(t->bt_s.bt_d.d_cache, (int) (h->h_pgno)) < 0)
			return (RET_ERROR);
	}
	return (RET_SUCCESS);
}

/*
 *  _BT_WRTMETA -- Write metadata to the btree.
 *
 *	Parameters:
 *		t -- tree to which to write
 *
 *	Returns:
 *		RET_SUCCESS, RET_ERROR.
 */

int
_bt_wrtmeta(t)
	BTREE_P t;
{
	BTMETA m;

	if (lseek(t->bt_s.bt_d.d_fd, 0l, L_SET) != 0l)
		return (RET_ERROR);

	/* lorder has to be in host-independent format */
	m.m_lorder = (u_long) htonl((long) t->bt_lorder);

	m.m_magic = BTREEMAGIC;
	m.m_version = BTREEVERSION;
	m.m_psize = t->bt_psize;
	m.m_free = t->bt_free;
	m.m_flags = t->bt_flags & BTF_NODUPS;

	if (t->bt_lorder != BYTE_ORDER) {
		BLSWAP(m.m_magic);
		BLSWAP(m.m_version);
		BLSWAP(m.m_psize);
		BLSWAP(m.m_free);
		BLSWAP(m.m_flags);
	}

	if (write(t->bt_s.bt_d.d_fd, (char *) &m, sizeof(m))
	    != sizeof(m)) {
		return (RET_ERROR);
	}

	t->bt_flags |= BTF_METAOK;

	return (RET_SUCCESS);
}
