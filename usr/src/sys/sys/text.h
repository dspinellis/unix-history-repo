/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)text.h	7.7 (Berkeley) 6/28/90
 */

/*
 * Text structure.
 * One allocated per pure
 * procedure on swap device.
 * Manipulated by text.c
 */
#define	NXDAD	12		/* param.h:MAXTSIZ / vmparam.h:DMTEXT */

struct text
{
	struct	text *x_forw;	/* forward link in free list */
	struct	text **x_back;	/* backward link in free list */
	swblk_t	x_daddr[NXDAD];	/* disk addresses of dmtext-page segments */
	swblk_t	x_ptdaddr;	/* disk address of page table */
	segsz_t x_size;		/* size (clicks) */
	struct proc *x_caddr;	/* ptr to linked proc, if loaded */
	struct vnode *x_vptr;	/* vnode of prototype */
	time_t	x_mtime;	/* last time modified */
	short	x_rssize;
	short	x_swrss;
	short	x_count;	/* reference count */
	short	x_ccount;	/* number of loaded references */
	char	x_flag;		/* traced, written flags */
	char	x_slptime;
	short	x_poip;		/* page out in progress count */
#if defined(tahoe)
	short	x_ckey;		/* code cache key */
#endif
};

#ifdef	KERNEL
struct	text *text, *textNTEXT;
int	ntext;
#endif

#define	XTRC	0x01		/* Text may be written, exclusive use */
#define	XWRIT	0x02		/* Text written into, must swap out */
#define	XLOAD	0x04		/* Currently being read from file */
#define	XLOCK	0x08		/* Being swapped in or out */
#define	XWANT	0x10		/* Wanted for swapping */
#define	XPAGV	0x20		/* Page in on demand from vnode */
#define	XUNUSED	0x40		/* Unused since swapped out for cache */
#define	XCACHED	0x80		/* Cached but not sticky */

/*
 * Text table statistics
 */
struct xstats {
	u_long	alloc;			/* calls to xalloc */
	u_long	alloc_inuse;		/*	found in use/sticky */
	u_long	alloc_cachehit;		/*	found in cache */
	u_long	alloc_cacheflush;	/*	flushed cached text */
	u_long	alloc_unused;		/*	flushed unused cached text */
	u_long	free;			/* calls to xfree */
	u_long	free_inuse;		/*	still in use/sticky */
	u_long	free_cache;		/*	placed in cache */
	u_long	free_cacheswap;		/*	swapped out to place in cache */
	u_long	purge;			/* calls to xpurge */
};
