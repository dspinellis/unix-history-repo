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
char sccsid[] = "@(#)lrucache.c	5.3 (Berkeley) 2/22/91";
#endif /* LIBC_SCCS and not lint */

/*
 *  lrucache.c -- LRU cache for disk-based btree pages.
 *
 *	This file implements an LRU cache in user space for disk-based
 *	btrees.
 */
#include <sys/param.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "lrucache.h"

/*
 *  LRUINIT -- Initialize a new LRU cache.
 *
 *	There's a separate LRU cache for every open file descriptor on which
 *	the user wants caching.  The desired cache size and logical page
 *	size are passed in.  We try to avoid growing the cache beyond the
 *	limit specified by the user, but if we cannot satisfy a block request
 *	without growing the cache, we do so.
 *
 *	Note that the page size passed in is the logical page size for
 *	use with this file descriptor, and doesn't necessarily have anything
 *	to do with the underlying hardware page size.
 *
 *	Parameters:
 *		fd -- file descriptor for this cache
 *		cachesz -- number of buffers in cache (suggested)
 *		pagesz -- logical page size inside this file
 *		inproc -- routine to call when a buffer is read
 *		outproc -- routine to call when a buffer is written
 *
 *	Returns:
 *		Opaque pointer to the LRU cache on success, NULL on failure.
 *
 *	Side Effects:
 *		Allocates memory for the hash table and LRU cache.  Buffers
 *		are allocated on demand, later.
 */
LRU
lruinit(fd, cachesz, pagesz, opaque, inproc, outproc)
	int fd;
	int cachesz;
	int pagesz;
	char *opaque;
	int (*inproc)();
	int (*outproc)();
{
	register LRUCACHE *l;
	int nbytes;

	/* allocate the LRU cache struct */
	if ((l = (LRUCACHE *) malloc((unsigned) sizeof(LRUCACHE)))
	    == (LRUCACHE *) NULL)
		return ((LRU) NULL);

	/* allocate the hash table */
	nbytes = cachesz * sizeof(CACHE_ENT *);
	if ((l->lru_cache = (CACHE_ENT **) malloc((unsigned) nbytes))
	    == (CACHE_ENT **) NULL) {
		(void) free((char *) l);
		return ((LRU) NULL);
	}

	/* init memory */
	bzero((char *) (l->lru_cache), (size_t) nbytes);
	l->lru_fd = fd;
	l->lru_psize = pagesz;
	l->lru_csize = cachesz;
	l->lru_cursz = 0;
	l->lru_opaque = opaque;
	l->lru_head = l->lru_tail = (LRU_ENT *) NULL;
	l->lru_inproc = inproc;
	l->lru_outproc = outproc;

	return ((LRU) l);
}

/*
 *  LRUGET -- Get a buffer from an LRU cache.
 *
 *	If the buffer is not in the cache at present, this routine will
 *	instantiate it from the file.  This REQUIRES that the desired
 *	block actually be on disk; we don't do non-blocking reads, so
 *	if it's not actually out there, this routine won't return for
 *	a very long time.  In order to instantiate a new buffer, use
 *	lrugetnew().
 *
 *	Parameters:
 *		lru -- the LRU cache to use.
 *		pgno -- the logical block number to get.
 *		nread -- pointer to an int, in which the number of bytes
 *			 read is returned.
 *
 *	Returns:
 *		(char *) pointer to the buffer for the desired block.  The
 *		number of bytes actually read is returned in *nread.
 *
 *	Warnings:
 *		The requested buffer is locked down until the user does
 *		an explicit lrurelease() on it.
 */

char *
lruget(lru, pgno, nread)
	LRU lru;
	int pgno;
	int *nread;
{
	LRUCACHE *l = (LRUCACHE *) lru;
	CACHE_ENT *ce;
	LRU_ENT *lruent;
	char *buffer;
	long pos;

	/* is it already in the cache? */
	if ((ce = lruhashget(l, pgno)) != (CACHE_ENT *) NULL) {

		/* yes, move it to the head and return it */
		lruent = ce->c_lruent;
		lruent->l_flags &= ~F_FREE;
		lruhead(l, ce->c_lruent);
		*nread = l->lru_psize;
		return (ce->c_lruent->l_buffer);
	}

	/* not there, get a free page */
	if ((buffer = lrugetpg(l, pgno, nread, lruget)) == (char *) NULL)
		return ((char *) NULL);

	/* okay, got a buffer -- fill it */
	pos = (long) (l->lru_psize * pgno);
	if (lseek(l->lru_fd, pos, L_SET) != pos)
		return ((char *) NULL);

	*nread = read(l->lru_fd, buffer, l->lru_psize);

	if (l->lru_inproc)
		(*(l->lru_inproc))(buffer, l->lru_opaque);

	return (buffer);
}

/*
 *  LRUGETNEW -- Get a page for a new block in an LRU cache.
 *
 *	This routine allows users to instantiate pages for a file if they
 *	don't exist on disk yet.  It's used to make a file bigger.
 *
 *	Parameters:
 *		lru -- the LRU cache to use
 *		pgno -- the (new) logical page number to instantiate
 *		nread -- ptr to int to get number of bytes read (this is
 *			 guaranteed to be zero, since the page isn't on disk)
 *
 *	Returns:
 *		Pointer to a buffer for the associated page, or NULL on
 *		failure.
 *
 *	Warnings:
 *		The associated buffer is locked down until the user
 *		explicitly does an lrurelease() on it.
 */

char *
lrugetnew(lru, pgno, nread)
	LRU lru;
	int pgno;
	int *nread;
{
	LRUCACHE *l = (LRUCACHE *) lru;

	*nread = 0;
	return (lrugetpg(l, pgno, nread, lrugetnew));
}

/*
 *  LRUFLUSH -- flush an LRU page to disk.
 *
 *	This routine forces a page to disk.  Users should use lruwrite(),
 *	which simply marks a page dirty and does late writing.
 *
 *	Parameters:
 *		l -- LRU cache
 *		lruent -- the LRU cache entry whose page we should flush
 *
 *	Returns:
 *		Zero on success, -1 on failure.
 */

lruflush(l, lruent)
	LRUCACHE *l;
	LRU_ENT *lruent;
{
	long pos;

	if (l->lru_outproc)
		(*(l->lru_outproc))(lruent->l_buffer, l->lru_opaque);

	pos = (long) (l->lru_psize * lruent->l_pgno);
	if (lseek(l->lru_fd, pos, L_SET) != pos)
		return (-1);
	if (write(l->lru_fd, lruent->l_buffer, l->lru_psize) != l->lru_psize)
		return (-1);

	if (l->lru_inproc)
		(*(l->lru_inproc))(lruent->l_buffer, l->lru_opaque);

	lruent->l_flags &= ~F_DIRTY;
	return (0);
}

/*
 *  LRUWRITE -- Mark an LRU cache buffer dirty
 *
 *	This routine is how users should move their changes to disk.  The
 *	cache code marks the associated buffer dirty, and flushes it to
 *	disk if we need to reuse the buffer for some other page.
 *
 *	Parameters:
 *		lru -- LRU cache
 *		pgno -- page number to flush
 *
 *	Returns:
 *		Zero on success, -1 on failure.
 */

int
lruwrite(lru, pgno)
	LRU lru;
	int pgno;
{
	LRUCACHE *l = (LRUCACHE *) lru;
	CACHE_ENT *ce;

	if ((ce = lruhashget(l, pgno)) == (CACHE_ENT *) NULL)
		return (-1);

	/* mark the entry dirty */
	ce->c_lruent->l_flags |= F_DIRTY;

	return (0);
}

/*
 *  LRUSYNC -- Flush all changes to disk
 *
 *	This routine allows users to force all changes to buffers currently
 *	in memory to disk.  This is expensive.
 *
 *	Parameters:
 *		lru -- LRU cache to flush
 *
 *	Returns:
 *		Zero on success, -1 on failure
 *
 *	Side Effects:
 *		After this call, all buffers are clean.
 */

int
lrusync(lru)
	LRU lru;
{
	LRUCACHE *l = (LRUCACHE *) lru;
	LRU_ENT *le;

	for (le = l->lru_head; le != (LRU_ENT *) NULL; le = le->l_next)
		if (le->l_flags & F_DIRTY)
			if (lruflush(l, le) < 0)
				return (-1);
	return (0);
}

/*
 *  LRURELEASE -- Release a buffer in the LRU cache for reuse
 *
 *	When the user does an lruget() or lrugetnew(), the buffer we return
 *	is locked down, to guarantee that it's not reused while the user
 *	still needs it.  Once a buffer is no longer needed, it should be
 *	released (via this routine) so that we can use it for other pages
 *	if necessary.
 *
 *	Parameters:
 *		lru -- LRU cache
 *		pgno -- page number of buffer to release
 *
 *	Returns:
 *		Zero on success, -1 on failure
 */

int
lrurelease(lru, pgno)
	LRU lru;
	int pgno;
{
	LRUCACHE *l = (LRUCACHE *) lru;
	CACHE_ENT *ce;

	if ((ce = lruhashget(l, pgno)) == (CACHE_ENT *) NULL)
		return (-1);
	ce->c_lruent->l_flags |= F_FREE;
	return (0);
}

/*
 *  LRUFREE -- Free an entire LRU cache
 *
 *	This routine releases an LRU cache.  The cache should not be
 *	used again.
 *
 *	Parameters:
 *		lru -- LRU cache to free
 *
 *	Returns:
 *		None.
 *
 *	Side Effects:
 *		Frees a lot of memory.
 */

void
lrufree(lru)
	LRU lru;
{
	LRUCACHE *l = (LRUCACHE *) lru;
	LRU_ENT *le;
	LRU_ENT *sle;

	for (le = l->lru_head; le != (LRU_ENT *) NULL; ) {
		free((char *) (le->l_buffer));
		sle = le;
		le = le->l_next;
		free((char *) sle);
	}
	free ((char *) l);
}
