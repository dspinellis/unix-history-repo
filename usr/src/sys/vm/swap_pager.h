/*
 * Copyright (c) 1990 University of Utah.
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)swap_pager.h	7.1 (Berkeley) %G%
 */

#ifndef	_SWAP_PAGER_
#define	_SWAP_PAGER_	1

/*
 * In the swap pager, the backing store for an object is organized as an
 * array of some number of "swap blocks".  A swap block consists of a bitmask
 * and some number of contiguous DEV_BSIZE disk blocks.  The minimum size
 * of a swap block is:
 *
 *	max(PAGE_SIZE, dmmin*DEV_BSIZE)			[ 32k currently ]
 *
 * bytes (since the pager interface is page oriented), the maximum size is:
 *
 *	min(#bits(swb_mask)*PAGE_SIZE, dmmax*DEV_BSIZE)	[ 128k currently ]
 *
 * where dmmin and dmmax are left over from the old VM interface.  The bitmask
 * (swb_mask) is used by swap_pager_haspage() to determine if a particular
 * page has actually been written; i.e. the pager copy of the page is valid.
 * All swap blocks in the backing store of an object will be the same size.
 *
 * The reason for variable sized swap blocks is to reduce fragmentation of
 * swap resources.  Whenever possible we allocate smaller swap blocks to
 * smaller objects.  The swap block size is determined from a table of
 * object-size vs. swap-block-size computed at boot time.
 */
typedef	int	sw_bm_t;	/* pager bitmask */

struct	swblock {
	sw_bm_t	 swb_mask;	/* bitmask of valid pages in this block */
	daddr_t	 swb_block;	/* starting disk block for this block */
};
typedef struct swblock	*sw_blk_t;

/*
 * Swap pager private data.
 */
struct swpager {
	vm_size_t    sw_osize;	/* size of object we are backing (bytes) */
	int	     sw_bsize;	/* size of swap blocks (DEV_BSIZE units) */
	int	     sw_nblocks;/* number of blocks in list (sw_blk_t units) */
	sw_blk_t     sw_blocks;	/* pointer to list of swap blocks */
	short	     sw_flags;	/* flags */
	short	     sw_poip;	/* pageouts in progress */
};
typedef struct swpager	*sw_pager_t;

#define	SW_WANTED	0x01
#define SW_NAMED	0x02

#ifdef KERNEL

void		swap_pager_init();
vm_pager_t	swap_pager_alloc();
void		swap_pager_dealloc();
boolean_t	swap_pager_getpage(), swap_pager_putpage();
boolean_t	swap_pager_haspage();

struct pagerops swappagerops = {
	swap_pager_init,
	swap_pager_alloc,
	swap_pager_dealloc,
	swap_pager_getpage,
	swap_pager_putpage,
	swap_pager_haspage
};

int		swap_pager_iodone();
boolean_t	swap_pager_clean();

#endif

#endif	/* _SWAP_PAGER_ */
