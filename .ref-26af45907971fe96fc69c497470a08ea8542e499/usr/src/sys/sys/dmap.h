/*-
 * Copyright (c) 1982, 1986, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)dmap.h	8.1 (Berkeley) %G%
 */

#ifndef _DMAP_H_
#define	_DMAP_H_

/*
 * Definitions for the mapping of vitual swap space to the physical swap
 * area - the disk map.
 */
#define	NDMAP	38		/* size of the swap area map */

struct dmap {
	swblk_t dm_size;	/* current size used by process */
	swblk_t dm_alloc;	/* amount of physical swap space allocated */
	swblk_t dm_map[NDMAP];	/* first disk block number in each chunk */
};
#ifdef KERNEL
struct dmap zdmap;
int dmmin, dmmax, dmtext;
#endif

/* The following structure is that ``returned'' from a call to vstodb(). */
struct dblock {
	swblk_t db_base;	/* base of physical contig drum block */
	swblk_t db_size;	/* size of block */
};
#endif	/* !_DMAP_H_ */
