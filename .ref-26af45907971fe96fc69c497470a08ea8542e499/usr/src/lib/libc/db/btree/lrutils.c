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
static char sccsid[] = "@(#)lrutils.c	5.2 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <stdlib.h>
#include <string.h>
#include "lrucache.h"

/*
 *  LRUGETPG -- Get a free page from the LRU cache.
 *
 *	This routine grows the cache if necessary, finds an unused page if
 *	it can, and handles flushing dirty buffers to disk.
 *
 *	One of the parameters to this routine (f) is the routine that called
 *	us.  If we have to grow the cache, we call this routine recursively
 *	in order to fill the buffer.  The reason for this is that we have
 *	two interfaces that call lrugetpg().  Lruget() fills a page from disk,
 *	and lrugetnew() just allocates a new (empty) page.
 *
 *	Parameters:
 *		l -- LRU cache to use.
 *		pgno -- page number for which we want a buffer
 *		nread -- pointer to an int to get number of bytes read
 *		f -- who called us
 *
 *	Returns:
 *		(char *) pointer to buffer to use, or NULL on failure.
 *
 *	Warnings:
 *		The buffer returned is locked down until the user does an
 *		explicit lrurelease() on it.
 */

char *
lrugetpg(l, pgno, nread, f)
	LRUCACHE *l;
	int pgno;
	int *nread;
	char *(*f)();
{
	CACHE_ENT *ce;
	LRU_ENT *lruent;
	char *buffer;

	/* if we're allowed to grow the cache, do so */
	if (l->lru_cursz < l->lru_csize) {

		/* get a buffer */
		if ((buffer = (char *) malloc((unsigned) l->lru_psize))
		    == (char *) NULL)
			return ((char *) NULL);

		/* get and LRU list entry */
		if ((lruent = (LRU_ENT *) malloc((unsigned) sizeof(LRU_ENT)))
		    == (LRU_ENT *) NULL)
			return ((char *) NULL);
		lruent->l_buffer = buffer;
		lruent->l_pgno = pgno;
		lruent->l_flags = NULL;

		/* manage spaghetti */
		lruent->l_prev = (LRU_ENT *) NULL;
		lruent->l_next = l->lru_head;
		if (l->lru_head != (LRU_ENT *) NULL)
			l->lru_head->l_prev = lruent;
		l->lru_head = lruent;
		if (l->lru_tail == (LRU_ENT *) NULL)
			l->lru_tail = lruent;

		/* add it to the hash table */
		ce = lruhashput(l, pgno, lruent);
		l->lru_cursz++;
	} else {
		lruent = l->lru_tail;

		/* find the oldest unused buffer */
		while (lruent != (LRU_ENT *) NULL
		       && !(lruent->l_flags & F_FREE))
			lruent = lruent->l_prev;

		/* if no free buffer was found, we have to grow the cache */
		if (lruent == (LRU_ENT *) NULL) {
			if (lrugrow(l) < 0)
				return ((char *) NULL);
			return ((*f)((LRU) l, pgno, nread));
		}

		/* okay, found a free buffer -- update hash table and list */
		ce = lruhashget(l, lruent->l_pgno);
		if (lruhashdel(l, lruent->l_pgno) < 0)
			return ((char *) NULL);

		/* flush the old page to disk, if necessary */
		if (lruent->l_flags & F_DIRTY)
			if (lruflush(l, lruent) < 0)
				return ((char *) NULL);

		/* update stats, hash table, and list */
		lruent->l_pgno = pgno;
		lruhead(l, lruent);
		ce = lruhashput(l, pgno, lruent);
		buffer = lruent->l_buffer;
	}
#ifdef lint
	ce = ce;
#endif /* lint */

	/* lock this page down */
	lruent->l_flags &= ~F_FREE;

	return (buffer);
}

/*
 *  LRUHEAD -- Move an LRU list entry to the head of the list.
 *
 *	The head of the list is where the most recently used item goes.
 *
 *	Parameters:
 *		l -- LRU cache
 *		lruent -- entry to move to the head of the list
 *
 *	Returns:
 *		None
 *
 *	Side Effects:
 *		Updates the cache's head and tail pointers as required.
 */

void
lruhead(l, lruent)
	LRUCACHE *l;
	LRU_ENT *lruent;
{
	LRU_ENT *next;
	LRU_ENT *prev;

	if (l->lru_head == lruent)
		return;

	next = lruent->l_next;
	prev = lruent->l_prev;
	lruent->l_prev = (LRU_ENT *) NULL;
	lruent->l_next = l->lru_head;
	l->lru_head->l_prev = lruent;
	l->lru_head = lruent;

	prev->l_next = next;
	if (next != (LRU_ENT *) NULL)
		next->l_prev = prev;

	if (l->lru_tail == lruent)
		l->lru_tail = prev;
}

/*
 *  LRUGROW -- Grow the LRU cache
 *
 *	This routine is called only if we can't satisfy a user's get() request
 *	using an existing buffer.  We need to rebuild the hash table so that
 *	subsequent lookups work correctly, since the cache size is used to
 *	compute hash keys.
 *
 *	Parameters:
 *		l -- LRU cache to grow
 *
 *	Returns:
 *		Zero on success, -1 on failure
 */

int
lrugrow(l)
	LRUCACHE *l;
{
	int oldsz, newsz;
	CACHE_ENT **new;
	CACHE_ENT *ce, *nce;
	int h;
	int i;

	oldsz = l->lru_csize;
	newsz = l->lru_csize + 1;

	/* get a new hash table */
	if ((new = (CACHE_ENT **) malloc((unsigned)newsz * sizeof(CACHE_ENT *)))
	    == (CACHE_ENT **) NULL)
		return (-1);

	/* build the new hash table */
	bzero((char *) new, (size_t) (newsz * sizeof(CACHE_ENT *)));
	for (i = oldsz; --i >= 0; ) {
		for (ce = l->lru_cache[i]; ce != (CACHE_ENT *) NULL; ) {
			nce = ce->c_chain;
			h = ce->c_pgno % newsz;
			ce->c_chain = new[h];
			new[h] = ce;
			ce = nce;
		}
	}

	/* get rid of the old hash table, and update the cache */
	free ((char *) (l->lru_cache));
	l->lru_cache = new;
	l->lru_csize = newsz;

	return (0);
}
