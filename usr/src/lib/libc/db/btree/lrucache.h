/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Mike Olson.
 *
 * %sccs.include.redist.c%
 */

/*
 * @(#)lrucache.h	5.1 (Berkeley) %G%
 */

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

#ifndef NULL
#define NULL	0
#endif /* ndef NULL */

/* this is the opaque type we return for LRU caches */
typedef	char	*LRU;

/* bits for l_flags in LRU_ENT structure */
#define	F_DIRTY		(1 << 0)
#define F_FREE		(1 << 1)

/* lru module routines */
extern CACHE_ENT	*lruhashget();
extern CACHE_ENT	*lruhashput();
extern int 		lruhashdel();
extern void		lruhead();
extern int 		lrugrow();
extern LRU		lruinit();
extern int		lruwrite();
extern int		lrusync();
extern char		*lruget();
extern char		*lrugetnew();
extern char		*lrugetpg();
extern int		lrurelease();
extern void		lrufree();
