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
static char sccsid[] = "@(#)lrucache.c	5.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

/*
 *  lrucache.c -- LRU cache for disk-based btree pages.
 *
 *	This file implements an LRU cache in user space for disk-based
 *	btrees.
 */
#include <sys/param.h>
#include <sys/file.h>
#include <sys/signal.h>

/*
 *  LRU list entries.  The head of the list is the most-recently requested
 *  block; the tail is the least-recently requested one.
 */

typedef struct LRU_ENT {
	char	*l_buffer;		/* buffer we return to user */
	int	l_pgno;			/* logical page number */
	int	l_flags;		/* FREE and DIRTY bits */
	struct LRU_ENT	*l_prev;	/* predecessor in LRU list */
	struct LRU_ENT	*l_next;	/* successor in LRU list */
} LRU_ENT;

/*
 *  Cache entries.  We use a hash table to avoid a linear walk of the LRU
 *  list when we need to look up blocks by number.  The hash table is
 *  chained.
 */

typedef struct CACHE_ENT {
	int			c_pgno;
	LRU_ENT			*c_lruent;
	struct CACHE_ENT	*c_chain;
} CACHE_ENT;

/*
 *  The LRU cache structure.  The cache size (lru_csize) is the largest size
 *  the user wants us to grow to; current size (lru_cursz) is always less than
 *  or equal to lru_csize.  Note that we will grow the cache (lru_csize) if
 *  it's the only way that we can satisfy a user's block request.
 */

typedef struct LRUCACHE {
	int		lru_fd;
	int		lru_csize;
	int		lru_psize;
	int		lru_cursz;
	char		*lru_opaque;		/* passed to inproc, outproc */
	int		(*lru_inproc)();
	int		(*lru_outproc)();
	LRU_ENT		*lru_head;
	LRU_ENT		*lru_tail;
	CACHE_ENT	**lru_cache;
} LRUCACHE;

/* this is the opaque type we return for LRU caches */
typedef	char	*LRU;

/* bits for l_flags in LRU_ENT structure */
#define	F_DIRTY		(1 << 0)
#define F_FREE		(1 << 1)

#define HASH(l, pgno)	(pgno % l->lru_csize)

/* all of these are defined in this file */
static CACHE_ENT	*lruhashget();
static CACHE_ENT	*lruhashput();
static int 		lruhashdel();
static void		lruhead();
static int 		lrugrow();
extern LRU		lruinit();
extern int		lruwrite();
extern int		lrusync();
extern char		*lruget();
extern char		*lrugetnew();
static char		*lrugetpg();
extern int		lrurelease();
extern void		lrufree();

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
		(void) free(l);
		return ((LRU) NULL);
	}

	/* init memory */
	bzero(l->lru_cache, nbytes);
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
	int hashind;
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

static char *
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

	/* lock this page down */
	lruent->l_flags &= ~F_FREE;

	return (buffer);
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
	int nbytes;

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
 *  LRUHASHGET -- Look up a block in the LRU cache by page number.
 *
 *	Parameters:
 *		l -- LRU cache
 *		pgno -- number of the logical page to get
 *
 *	Returns:
 *		(CACHE_ENT *) pointer to the associated hash table entry
 *		(if any), or NULL (if none).
 */

static CACHE_ENT *
lruhashget(l, pgno)
	LRUCACHE *l;
	int pgno;
{
	int hashind;
	CACHE_ENT *ce;

	hashind = HASH(l, pgno);

	/* walk the chain */
	for (ce = l->lru_cache[hashind];
	     ce != (CACHE_ENT *) NULL;
	     ce = ce->c_chain) {
		if (ce->c_pgno == pgno)
			return (ce);
	}

	return ((CACHE_ENT *) NULL);
}

/*
 *  LRUHASHPUT -- Add an entry for a given page to the cache hash table.
 *
 *	This routine assumes that the given page does not yet have an entry
 *	in the table.
 *
 *	Parameters:
 *		l -- LRU cache
 *		pgno -- logical page number for this entry
 *		lruent -- LRU list entry at which hash table entry should
 *			  point
 *
 *	Returns:
 *		(CACHE_ENT *) pointer to the new hash table entry on success,
 *		or NULL on failure.
 *
 *	Side Effects:
 *		Allocates memory which should be freed when the hash table
 *		entry is removed.
 */

static CACHE_ENT *
lruhashput(l, pgno, lruent)
	LRUCACHE *l;
	int pgno;
	LRU_ENT *lruent;
{
	int hashind;
	CACHE_ENT *ce;

	if ((ce = (CACHE_ENT *) malloc((unsigned) sizeof(CACHE_ENT)))
	    == (CACHE_ENT *) NULL)
		return ((CACHE_ENT *) NULL);

	hashind = HASH(l, pgno);

	ce->c_pgno = pgno;
	ce->c_lruent = lruent;
	ce->c_chain = l->lru_cache[hashind];
	l->lru_cache[hashind] = ce;

	return (ce);
}

/*
 *  LRUHASHDEL -- Delete the entry for a given page from the LRU cache
 *		  hash table.
 *
 *	Parameters:
 *		l -- LRU cache
 *		pgno -- page number for which we should delete htable entry
 *
 *	Returns:
 *		Zero on success, -1 on failure.
 *
 *	Side Effects:
 *		Releases the memory occupied by the hash table entry.
 */

static int
lruhashdel(l, pgno)
	LRUCACHE *l;
	int pgno;
{
	CACHE_ENT *ce;
	CACHE_ENT *sce;
	int hashind;

	sce = (CACHE_ENT *) NULL;
	hashind = HASH(l, pgno);

	/* find the entry we want to delete */
	for (ce = l->lru_cache[hashind];
	     ce != (CACHE_ENT *) NULL;
	     ce = ce->c_chain) {
		if (ce->c_pgno == pgno)
			break;
		sce = ce;
	}

	if (ce == (CACHE_ENT *) NULL)
		return (-1);

	/* remove it from the chain */
	if (sce == (CACHE_ENT *) NULL)
		l->lru_cache[hashind] = ce->c_chain;
	else
		sce->c_chain = ce->c_chain;

	/* release it */
	free (ce);

	/* success */
	return (0);
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

static void
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
	LRU_ENT *lruent;
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

static int
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
	bzero(new, (newsz * sizeof(CACHE_ENT *)));
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
	free (l->lru_cache);
	l->lru_cache = new;
	l->lru_csize = newsz;

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
		free(le->l_buffer);
		sle = le;
		le = le->l_next;
		free(sle);
	}
	free (l);
}
