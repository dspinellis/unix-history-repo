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

/*
 * @(#)lrucache.h	5.1 (Berkeley) 1/23/91
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
