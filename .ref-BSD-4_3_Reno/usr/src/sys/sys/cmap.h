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
 *	@(#)cmap.h	7.5 (Berkeley) 6/28/90
 */

/*
 * core map entry
 *
 * Limits imposed by this structure:
 *
 *		limit		     cur. size		fields
 *	Physical memory*		64K/256K*CLSIZE	c_next, c_prev, c_hlink
 *	Mounted filesystems		255		c_mdev
 *	size of a process segment	1 Gb		c_page
 *	filesystem size			8 Gb		c_blkno
 *	proc, text table size		64K/16K		c_ndx
 *
 *	* memory is configured for up to 64K clusters (64Mb with 1K clusters)
 *	using u_short cmap indices, or up to 256K clusters (256Mb) if MAXMEM
 *	is defined in the configuration file to a value larger than
 *	64K*CLSIZE.  This definition reduces the max proc/text table size
 *	to 16K entries.
 *
 *	UNTESTED with MAXMEM > 64Mb
 */

#define	MAXMEM_SHORT	64*1024*CLSIZE	/* maximum memory for short indices */
#define	MAXMEM_18	256*1024*CLSIZE	/* maximum memory for 18-bit indices */
#ifndef MAXMEM
#define	MAXMEM		MAXMEM_SHORT	/* maximum memory, in pages */
#endif
#if MAXMEM > MAXMEM_18
#undef MAXMEM
#define	MAXMEM		MAXMEM_18	/* maximum memory, in pages */
#endif

#ifndef	LOCORE
struct cmap
{
#if MAXMEM <= MAXMEM_SHORT
unsigned short 	c_next,		/* index of next free list entry */
		c_prev,		/* index of previous free list entry */
		c_hlink;	/* hash link for <blkno,mdev> */
unsigned short	c_ndx;		/* index of owner proc or text */

#else /* MAXMEM */
unsigned long	c_hlink,	/* hash link for <blkno,mdev> */
		c_next;		/* index of next free list entry */
unsigned int 	c_prev:18,	/* index of previous free list entry */
		c_ndx:14;	/* index of owner proc or text */
#endif /* MAXMEM */

unsigned int	c_page:21,	/* virtual page number in segment */
		c_lock:1,	/* locked for raw i/o or pagein */
		c_want:1,	/* wanted */
		c_intrans:1,	/* intransit bit */
		c_free:1,	/* on the free list */
		c_gone:1,	/* associated page has been released */
		c_type:2,	/* type CSYS or CTEXT or CSTACK or CDATA */
		:4;		/* to longword boundary */
daddr_t		c_blkno;	/* disk block this is a copy of */
struct vnode	*c_vp;		/* vnode to which c_blkno refers */
};
#else	LOCORE
/*
 * bit offsets of elements in cmap
 */
#if MAXMEM <= MAXMEM_SHORT
#define	C_INTRANS	87
#define	C_FREE		88
#else
#define	C_INTRANS	119
#define	C_FREE		120
#endif
#endif	LOCORE

#define	CMHEAD	0

/*
 * Shared text pages are not totally abandoned when a process
 * exits, but are remembered while in the free list hashed by <vp,blkno>
 * off the cmhash structure so that they can be reattached
 * if another instance of the program runs again soon.
 */
#if MAXMEM <= MAXMEM_SHORT
#define	CMHSIZ	2048		/* SHOULD BE DYNAMIC */
#else
#define	CMHSIZ	(16*1024)	/* SHOULD BE DYNAMIC */
#endif
#define	CMHASH(bn)	((bn)&(CMHSIZ-1))

#ifndef	LOCORE
#ifdef	KERNEL
struct	cmap *cmap;
struct	cmap *ecmap;
int	ncmap;
struct	cmap *mfind();
int	firstfree, maxfree;
int	ecmx;			/* cmap index of ecmap */
u_long	cmhash[CMHSIZ];
#endif

/* bits defined in c_type */

#define	CSYS		0		/* none of below */
#define	CTEXT		1		/* belongs to shared text segment */
#define	CDATA		2		/* belongs to data segment */
#define	CSTACK		3		/* belongs to stack segment */

#define	pgtocm(x)	(((int) ((x)-firstfree) / CLSIZE) + 1)
#define	cmtopg(x)	((((x)-1) * CLSIZE) + firstfree)
#endif	LOCORE
