/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)text.h	7.2 (Berkeley) 10/13/86
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
	size_t	x_size;		/* size (clicks) */
	struct proc *x_caddr;	/* ptr to linked proc, if loaded */
	struct inode *x_iptr;	/* inode of prototype */
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
#define	XPAGI	0x20		/* Page in on demand from inode */
#define	XUNUSED	0x40		/* unused since swapped out for cache */

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
};
