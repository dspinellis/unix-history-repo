/*
 * Copyright (c) 1990 University of Utah.
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
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
 *
 * from: Utah $Hdr: swap_pager.c 1.4 91/04/30$
 * from: @(#)swap_pager.c	7.4 (Berkeley) 5/7/91
 *
 * $Id: swap_pager.c,v 1.12 1994/01/14 16:27:12 davidg Exp $
 */

/*
 * Mostly rewritten by John Dyson with help from David Greenman, 12-Jan-1994
 */

#include "param.h"
#include "proc.h"
#include "buf.h"
#include "kernel.h"
#include "systm.h"
#include "specdev.h"
#include "vnode.h"
#include "malloc.h"
#include "queue.h"
#include "rlist.h"

#include "vm_param.h"
#include "queue.h"
#include "lock.h"
#include "vm.h"
#include "vm_prot.h"
#include "vm_object.h"
#include "vm_page.h"
#include "vm_pageout.h"
#include "swap_pager.h"
#include "vm_map.h"

#ifndef NPENDINGIO
#define NPENDINGIO	96
#endif

extern int nswbuf;
int nswiodone;
extern int vm_pageout_rate_limit;
static int cleandone;
extern int hz;
int swap_pager_full;
extern vm_map_t pager_map;
extern int vm_pageout_pages_needed;

struct swpagerclean {
	queue_head_t		spc_list;
	int			spc_flags;
	struct buf		*spc_bp;
	sw_pager_t		spc_swp;
	vm_offset_t		spc_kva;
	vm_page_t		spc_m;
} swcleanlist [NPENDINGIO] ;

typedef	struct swpagerclean	*swp_clean_t;

extern vm_map_t kernel_map;

/* spc_flags values */
#define SPC_ERROR	0x01

#define SWB_EMPTY (-1)

queue_head_t	swap_pager_done;	/* list of compileted page cleans */
queue_head_t	swap_pager_inuse;	/* list of pending page cleans */
queue_head_t	swap_pager_free;	/* list of free pager clean structs */
queue_head_t	swap_pager_list;	/* list of "named" anon regions */
queue_head_t	swap_pager_un_list;	/* list of "unnamed" anon pagers */
#define	SWAP_FREE_NEEDED	0x1	/* need a swap block */
int swap_pager_needflags;

static queue_head_t *swp_qs[]={
	&swap_pager_list, &swap_pager_un_list, (queue_head_t *) 0
};

struct pagerops swappagerops = {
	swap_pager_init,
	swap_pager_alloc,
	swap_pager_dealloc,
	swap_pager_getpage,
	swap_pager_getmulti,
	swap_pager_putpage,
	swap_pager_haspage
};

extern int nswbuf;

int npendingio = NPENDINGIO;
int swiopend;
int pendingiowait;
int require_swap_init;
void swap_pager_finish();
int dmmin, dmmax;
extern int vm_page_count;

struct buf * getpbuf() ;
void relpbuf(struct buf *bp) ;

void
swap_pager_init()
{
	register int i;
	extern int dmmin, dmmax;

	dfltpagerops = &swappagerops;
	queue_init(&swap_pager_list);
	queue_init(&swap_pager_un_list);

	/*
	 * Initialize clean lists
	 */
	queue_init(&swap_pager_inuse);
	queue_init(&swap_pager_done);
	queue_init(&swap_pager_free);

	require_swap_init = 1;

	/*
	 * Calculate the swap allocation constants.
	 */

	dmmin = CLBYTES/DEV_BSIZE;
	dmmax = btodb(SWB_NPAGES*NBPG)*2;

}

/*
 * Allocate a pager structure and associated resources.
 * Note that if we are called from the pageout daemon (handle == NULL)
 * we should not wait for memory as it could resulting in deadlock.
 */
vm_pager_t
swap_pager_alloc(handle, size, prot)
	caddr_t handle;
	register vm_size_t size;
	vm_prot_t prot;
{
	register vm_pager_t pager;
	register sw_pager_t swp;
	int waitok;
	int i,j;
			
	if (require_swap_init) {
		register swp_clean_t spc;
		if (npendingio > nswbuf)
			npendingio = nswbuf;
		if (npendingio > vm_page_count / 32)
			npendingio = vm_page_count / 32;
		/*
		 * kva's are allocated here so that we dont need to keep
		 * doing kmem_alloc pageables at runtime
		 */
		for (i = 0, spc = swcleanlist; i < npendingio ; i++, spc++) {
			spc->spc_kva = kmem_alloc_pageable(pager_map, NBPG);
			if (!spc->spc_kva)
				break;
			spc->spc_flags = 0;
			queue_enter(&swap_pager_free, spc, swp_clean_t, spc_list);
		}
		require_swap_init = 0;
	}
		
	/*
	 * If this is a "named" anonymous region, look it up and
	 * return the appropriate pager if it exists.
	 */
	if (handle) {
		pager = vm_pager_lookup(&swap_pager_list, handle);
		if (pager != NULL) {
			/*
			 * Use vm_object_lookup to gain a reference
			 * to the object and also to remove from the
			 * object cache.
			 */
			if (vm_object_lookup(pager) == NULL)
				panic("swap_pager_alloc: bad object");
			return(pager);
		}
	}

	if (swap_pager_full)
		return(NULL);

	/*
	 * Pager doesn't exist, allocate swap management resources
	 * and initialize.
	 */
	waitok = handle ? M_WAITOK : M_NOWAIT; 
	pager = (vm_pager_t)malloc(sizeof *pager, M_VMPAGER, waitok);
	if (pager == NULL)
		return(NULL);
	swp = (sw_pager_t)malloc(sizeof *swp, M_VMPGDATA, waitok);
	if (swp == NULL) {
		free((caddr_t)pager, M_VMPAGER);
		return(NULL);
	}
	size = round_page(size);
	swp->sw_osize = size;
	swp->sw_nblocks = (btodb(size) + btodb(SWB_NPAGES * NBPG) - 1) / btodb(SWB_NPAGES*NBPG);
	swp->sw_blocks = (sw_blk_t)
		malloc(swp->sw_nblocks*sizeof(*swp->sw_blocks),
		       M_VMPGDATA, waitok);
	if (swp->sw_blocks == NULL) {
		free((caddr_t)swp, M_VMPGDATA);
		free((caddr_t)pager, M_VMPAGER);
		return(NULL);
	}
	bzero((caddr_t)swp->sw_blocks,
	      swp->sw_nblocks * sizeof(*swp->sw_blocks));

	for (i = 0; i < swp->sw_nblocks; i++) {
		for (j = 0; j < SWB_NPAGES; j++)
			swp->sw_blocks[i].swb_block[j] = SWB_EMPTY;
	}

	swp->sw_poip = 0;
	if (handle) {
		vm_object_t object;

		swp->sw_flags = SW_NAMED;
		queue_enter(&swap_pager_list, pager, vm_pager_t, pg_list);
		/*
		 * Consistant with other pagers: return with object
		 * referenced.  Can't do this with handle == NULL
		 * since it might be the pageout daemon calling.
		 */
		object = vm_object_allocate(size);
		vm_object_enter(object, pager);
		vm_object_setpager(object, pager, 0, FALSE);
	} else {
		swp->sw_flags = 0;
		queue_init(&pager->pg_list);
		queue_enter(&swap_pager_un_list, pager, vm_pager_t, pg_list);
	}
	pager->pg_handle = handle;
	pager->pg_ops = &swappagerops;
	pager->pg_type = PG_SWAP;
	pager->pg_data = (caddr_t)swp;

	return(pager);
}

/*
 * returns disk block associated with pager and offset
 * additionally, as a side effect returns a flag indicating
 * if the block has been written
 */

static int *
swap_pager_diskaddr(swp, offset, valid)
	sw_pager_t swp;
	vm_offset_t offset;
	int *valid;
{
	register sw_blk_t swb;
	int ix;

	if (valid)
		*valid = 0;
	ix = offset / (SWB_NPAGES*NBPG);
	if (swp->sw_blocks == NULL || ix >= swp->sw_nblocks) {
		return(FALSE);
	}
	swb = &swp->sw_blocks[ix];
	ix = (offset % (SWB_NPAGES*NBPG)) / NBPG;
	if (valid)
		*valid = swb->swb_valid & (1<<ix);
	return &swb->swb_block[ix];
}

/*
 * Utility routine to set the valid (written) bit for
 * a block associated with a pager and offset
 */
static void
swap_pager_setvalid(swp, offset, valid)
	sw_pager_t swp;
	vm_offset_t offset;
	int valid;
{
	register sw_blk_t swb;
	int ix;
	
	ix = offset / (SWB_NPAGES*NBPG);
	if (swp->sw_blocks == NULL || ix >= swp->sw_nblocks) 
		return;

	swb = &swp->sw_blocks[ix];
	ix = (offset % (SWB_NPAGES*NBPG)) / NBPG;
	if (valid)
		swb->swb_valid |= (1 << ix);
	else
		swb->swb_valid &= ~(1 << ix);
	return;
}

/*
 * this routine frees swap blocks from a specified pager
 */
void
swap_pager_freespace(pager, start, size)
	vm_pager_t pager;
	vm_offset_t start;
	vm_offset_t size;
{
	sw_pager_t swp = (sw_pager_t) pager->pg_data;
	vm_offset_t i;
	int s;

	s = splbio();
	for (i = start; i < round_page(start + size - 1); i += NBPG) {
		int *addr = swap_pager_diskaddr(swp, i, 0);
		if (addr && *addr != SWB_EMPTY) {
			rlist_free(&swapmap, *addr, *addr + btodb(NBPG) - 1);
			*addr = SWB_EMPTY;
			swap_pager_full = 0;
		}
	}
	splx(s);
}

/*
 * swap_pager_reclaim frees up over-allocated space from all pagers
 * this eliminates internal fragmentation due to allocation of space
 * for segments that are never swapped to. It has been written so that
 * it does not block until the rlist_free operation occurs; it keeps
 * the queues consistant.
 */

/*
 * Maximum number of blocks (pages) to reclaim per pass
 */
#define MAXRECLAIM 256

void
swap_pager_reclaim()
{
	vm_pager_t p;
	sw_pager_t swp;
	int i, j, k;
	int s;
	int reclaimcount;
	static int reclaims[MAXRECLAIM];
	static int in_reclaim;

	s = splbio();
	if (in_reclaim) {
		tsleep((caddr_t) &in_reclaim, PSWP, "swrclm", 0);
		splx(s);
		return;
	}
	in_reclaim = 1;
	reclaimcount = 0;

	/* for each pager queue */
	for (k = 0; swp_qs[k]; k++) {

		p = (vm_pager_t) queue_first(swp_qs[k]);
		while (reclaimcount < MAXRECLAIM &&
			!queue_end(swp_qs[k], (queue_entry_t) p)) {

			/*
			 * see if any blocks associated with a pager has been
			 * allocated but not used (written)
			 */
			swp = (sw_pager_t) p->pg_data;
			for (i = 0; i < swp->sw_nblocks; i++) {
				sw_blk_t swb = &swp->sw_blocks[i];
				for (j = 0; j < SWB_NPAGES; j++) {
					if (swb->swb_block[j] != SWB_EMPTY &&
						(swb->swb_valid & (1 << j)) == 0) {
						reclaims[reclaimcount++] = swb->swb_block[j];
						swb->swb_block[j] = SWB_EMPTY;
						if (reclaimcount >= MAXRECLAIM)
							goto rfinished;
					}
				}
			}
			p = (vm_pager_t) queue_next(&p->pg_list);
		}
	}
	
rfinished:

/*
 * free the blocks that have been added to the reclaim list
 */
	for (i = 0; i < reclaimcount; i++) {
		rlist_free(&swapmap, reclaims[i], reclaims[i] + btodb(NBPG) - 1);
		wakeup((caddr_t) &in_reclaim);
		swap_pager_full = 0;
	}

	splx(s);
	in_reclaim = 0;
	wakeup((caddr_t) &in_reclaim);
}
		

/*
 * swap_pager_copy copies blocks from one pager to another and
 * destroys the source pager
 */

void
swap_pager_copy(srcpager, srcoffset, dstpager, dstoffset, offset)
	vm_pager_t srcpager;
	vm_offset_t srcoffset;
	vm_pager_t dstpager;
	vm_offset_t dstoffset;
	vm_offset_t offset;
{
	sw_pager_t srcswp, dstswp;
	vm_offset_t i;
	int s;

	srcswp = (sw_pager_t) srcpager->pg_data;
	dstswp = (sw_pager_t) dstpager->pg_data;

	s = splbio();
	if (srcswp->sw_flags & SW_NAMED) {
		queue_remove(&swap_pager_list, srcpager, vm_pager_t, pg_list);
		srcswp->sw_flags &= ~SW_NAMED;
	} else {
		queue_remove(&swap_pager_un_list, srcpager, vm_pager_t, pg_list);
	}
	
	while (srcswp->sw_poip) {
		tsleep((caddr_t)srcswp, PVM, "spgout", 0); 
	}
	splx(s);

	(void) swap_pager_clean(NULL, B_WRITE);
	
	s = splbio();
/*
 * clear source block before destination object
 */
	for (i = 0; i < offset + srcoffset; i += NBPG) {
		int *addr = swap_pager_diskaddr(srcswp, i, 0);
		if (addr && *addr != SWB_EMPTY) {
			rlist_free(&swapmap, *addr, *addr + btodb(NBPG) - 1);
			*addr = SWB_EMPTY;
			swap_pager_full = 0;
		}
	}
/*
 * transfer source to destination
 */
	for (i = 0; i < dstswp->sw_osize; i += NBPG) {
		int srcvalid, dstvalid;
		int *srcaddrp = swap_pager_diskaddr(srcswp, i + offset + srcoffset,
			&srcvalid);
		int *dstaddrp;
		if (srcaddrp && *srcaddrp != SWB_EMPTY) {
			if (srcvalid) {
				dstaddrp = swap_pager_diskaddr(dstswp, i + dstoffset, &dstvalid);
				if (!dstvalid && dstaddrp && *dstaddrp != SWB_EMPTY) {
					rlist_free(&swapmap, *dstaddrp, *dstaddrp + btodb(NBPG) - 1);
					*dstaddrp = SWB_EMPTY;
					swap_pager_full = 0;
				}
				if (dstaddrp && *dstaddrp == SWB_EMPTY) {
					*dstaddrp = *srcaddrp;
					*srcaddrp = SWB_EMPTY;
					swap_pager_setvalid(dstswp, i + dstoffset, 1);
				}
			}
			if (*srcaddrp != SWB_EMPTY) {
				rlist_free(&swapmap, *srcaddrp, *srcaddrp + btodb(NBPG) - 1);
				*srcaddrp = SWB_EMPTY;
				swap_pager_full = 0;
			}
		}
	}

/*
 * deallocate the rest of the source object
 */
	for (i = dstswp->sw_osize + offset + srcoffset; i < srcswp->sw_osize; i += NBPG) {
		int *srcaddrp = swap_pager_diskaddr(srcswp, i, 0);
		if (srcaddrp && *srcaddrp != SWB_EMPTY) {
			rlist_free(&swapmap, *srcaddrp, *srcaddrp + btodb(NBPG) - 1);
			*srcaddrp = SWB_EMPTY;
			swap_pager_full = 0;
		}
	}
				
	splx(s);

	free((caddr_t)srcswp->sw_blocks, M_VMPGDATA);
	srcswp->sw_blocks = 0;
	free((caddr_t)srcswp, M_VMPGDATA);
	srcpager->pg_data = 0;
	free((caddr_t)srcpager, M_VMPAGER);

	return;
}


void
swap_pager_dealloc(pager)
	vm_pager_t pager;
{
	register int i,j;
	register sw_blk_t bp;
	register sw_pager_t swp;
	int s;

	/*
	 * Remove from list right away so lookups will fail if we
	 * block for pageout completion.
	 */
	s = splbio();
	swp = (sw_pager_t) pager->pg_data;
	if (swp->sw_flags & SW_NAMED) {
		queue_remove(&swap_pager_list, pager, vm_pager_t, pg_list);
		swp->sw_flags &= ~SW_NAMED;
	} else {
		queue_remove(&swap_pager_un_list, pager, vm_pager_t, pg_list);
	}
	/*
	 * Wait for all pageouts to finish and remove
	 * all entries from cleaning list.
	 */

	while (swp->sw_poip) {
		tsleep((caddr_t)swp, PVM, "swpout", 0); 
	}
	splx(s);
		

	(void) swap_pager_clean(NULL, B_WRITE);

	/*
	 * Free left over swap blocks
	 */
	s = splbio();
	for (i = 0, bp = swp->sw_blocks; i < swp->sw_nblocks; i++, bp++) {
		for (j = 0; j < SWB_NPAGES; j++)
		if (bp->swb_block[j] != SWB_EMPTY) {
			rlist_free(&swapmap, (unsigned)bp->swb_block[j],
				(unsigned)bp->swb_block[j] + btodb(NBPG) - 1);
			bp->swb_block[j] = SWB_EMPTY;
			swap_pager_full = 0;
		}
	}
	splx(s);

	/*
	 * Free swap management resources
	 */
	free((caddr_t)swp->sw_blocks, M_VMPGDATA);
	swp->sw_blocks = 0;
	free((caddr_t)swp, M_VMPGDATA);
	pager->pg_data = 0;
	free((caddr_t)pager, M_VMPAGER);
}

int
swap_pager_getmulti(pager, m, count, reqpage, sync)
	vm_pager_t pager;
	vm_page_t *m;
	int count;
	int reqpage;
	boolean_t sync;
{
	return swap_pager_io((sw_pager_t) pager->pg_data, m, count, reqpage, B_READ);
}

int
swap_pager_getpage(pager, m, sync)
	vm_pager_t pager;
	vm_page_t m;
	boolean_t sync;
{
	vm_page_t marray[1];
	
	marray[0] = m;
	return swap_pager_io((sw_pager_t)pager->pg_data, marray, 1, 0, B_READ);
}

int
swap_pager_putpage(pager, m, sync)
	vm_pager_t pager;
	vm_page_t m;
	boolean_t sync;
{
	int flags;
	vm_page_t marray[1];


	if (pager == NULL) {
		(void) swap_pager_clean(NULL, B_WRITE);
		return VM_PAGER_OK;
	}
	
	marray[0] = m;
	flags = B_WRITE;
	if (!sync)
		flags |= B_ASYNC;
	return(swap_pager_io((sw_pager_t)pager->pg_data, marray, 1, 0, flags));
}

static inline int
swap_pager_block_index(swp, offset)
	sw_pager_t swp;
	vm_offset_t offset;
{
	return (offset / (SWB_NPAGES*NBPG));
}

static inline int
swap_pager_block_offset(swp, offset)
	sw_pager_t swp;
	vm_offset_t offset;
{	
	return (offset % (SWB_NPAGES*NBPG));
}

static boolean_t
_swap_pager_haspage(swp, offset)
	sw_pager_t swp;
	vm_offset_t offset;
{
	register sw_blk_t swb;
	int ix;

	ix = offset / (SWB_NPAGES*NBPG);
	if (swp->sw_blocks == NULL || ix >= swp->sw_nblocks) {
		return(FALSE);
	}
	swb = &swp->sw_blocks[ix];
	ix = (offset % (SWB_NPAGES*NBPG)) / NBPG;
	if (swb->swb_block[ix] != SWB_EMPTY) {
		if (swb->swb_valid & (1 << ix))
			return TRUE;
	}

	return(FALSE);
}

boolean_t
swap_pager_haspage(pager, offset)
	vm_pager_t pager;
	vm_offset_t offset;
{
	return _swap_pager_haspage((sw_pager_t) pager->pg_data, offset);
}

static void
swap_pager_freepage(m)
	vm_page_t m;
{
	PAGE_WAKEUP(m);
	vm_page_free(m);
}

static void
swap_pager_ridpages(m, count, reqpage)
	vm_page_t *m;
	int count;
	int reqpage;
{
	int i;
	int s;

	s = splhigh();
	for (i = 0; i < count; i++)
		if (i != reqpage)
			swap_pager_freepage(m[i]);
	splx(s);
}

int swapwritecount=0;

void
swap_pager_iodone1(bp)
	struct buf *bp;
{
	bp->b_flags |= B_DONE;
	bp->b_flags &= ~B_ASYNC;
	wakeup((caddr_t)bp);
	if ((bp->b_flags & B_READ) == 0)
		vwakeup(bp);
}
/*
 * Scaled down version of swap().
 * BOGUS:  lower level IO routines expect a KVA so we have to map our
 * provided physical page into the KVA to keep them happy.
 */
int
swap_pager_io(swp, m, count, reqpage, flags)
	register sw_pager_t swp;
	vm_page_t *m;
	int count, reqpage;
	int flags;
{
	register struct buf *bp;
	register sw_blk_t swb;
	register int s;
	int i, ix;
	boolean_t rv;
	vm_offset_t kva, off;
	swp_clean_t spc;
	int cluster;
	vm_offset_t paging_offset;
	vm_object_t object;
	int reqaddr, mydskregion;
	extern int dmmin, dmmax;

	spc = NULL;

	object = m[reqpage]->object;
	paging_offset = object->paging_offset;
	/*
	 * First determine if the page exists in the pager if this is
	 * a sync read.  This quickly handles cases where we are
	 * following shadow chains looking for the top level object
	 * with the page.
	 */
	off = m[reqpage]->offset + paging_offset;
	ix = swap_pager_block_index(swp, off);
	if (swp->sw_blocks == NULL || ix >= swp->sw_nblocks) {
		/* printf("swap pager: out of range\n"); */
		swap_pager_ridpages(m, count, reqpage);
		return(VM_PAGER_FAIL);
	}
	

	swb = &swp->sw_blocks[ix];
	off = swap_pager_block_offset(swp, off) / NBPG;
	reqaddr = swb->swb_block[off];

	/* make sure that our I/O request is contiguous */
	if (flags & B_READ) {
		int first = 0, last = count;
		int failed = 0;
		int reqdskregion = reqaddr / dmmax;
		int valid;

		if (reqaddr == SWB_EMPTY ||
			(swb->swb_valid & (1 << off)) == 0) {
			swap_pager_ridpages(m, count, reqpage);
			return(VM_PAGER_FAIL);
		}
				
		/*
		 * search backwards for the first contiguous page to transfer
		 */
		for (i = reqpage - 1; i >= 0; --i) {
			int *tmpaddr = swap_pager_diskaddr(swp,
				m[i]->offset + paging_offset,&valid);
			if (tmpaddr == 0 || failed || !valid ||
				*tmpaddr != reqaddr + btodb((i - reqpage) * NBPG)) {
				failed = 1;
				swap_pager_freepage(m[i]);
				m[i] = 0;
				if (first == 0)
					first = i + 1;
			} else {
				mydskregion = *tmpaddr / dmmax;
				if (mydskregion != reqdskregion) {
					failed = 1;
					swap_pager_freepage(m[i]);
					m[i] = 0;
					first = i + 1;
				}
			}
		}
		/*
		 * search forwards for the last contiguous page to transfer
		 */
		failed = 0;
		for (i = reqpage + 1; i < count; i++) {
			int *tmpaddr = swap_pager_diskaddr(swp, m[i]->offset + paging_offset,0);
			if (tmpaddr == 0 || failed || !valid ||
				*tmpaddr != reqaddr + btodb((i - reqpage) * NBPG) ) {
				failed = 1;
				swap_pager_freepage(m[i]);
				m[i] = 0;
				if (last == count)
					last = i;
			} else {
				mydskregion = *tmpaddr / dmmax;
				if (mydskregion != reqdskregion) {
					failed = 1;
					swap_pager_freepage(m[i]);
					m[i] = 0;
					if (last == count)
						last = i;
				}
			}
		}
		count = last;
		if (first != 0) {
			for (i = first; i < count; i++) {
				m[i - first] = m[i];
			}
			count -= first;
			reqpage -= first;
		}
	}
	
	/*
	 * For reads (pageins) and synchronous writes, we clean up
	 * all completed async pageouts.
	 */
	if ((flags & B_ASYNC) == 0) {
		swap_pager_clean(NULL, flags);
	}
	/*
	 * For async writes (pageouts), we cleanup completed pageouts so
	 * that all available resources are freed.  Also tells us if this
	 * page is already being cleaned.  If it is, or no resources
	 * are available, we try again later.
	 */
	else if (swap_pager_clean(m[reqpage], B_WRITE)) {
		swap_pager_ridpages(m, count, reqpage);
		return VM_PAGER_TRYAGAIN;
	}

	spc = NULL;	/* we might not use an spc data structure */
	kva = 0;

	/*
	 * we allocate a new kva for transfers > 1 page
	 * but for transfers == 1 page, the swap_pager_free list contains
	 * entries that have pre-allocated kva's
	 */
	if ((flags & B_READ) && count > 1) {
		kva = kmem_alloc_pageable(pager_map, count*NBPG);
	}
		
	if (!kva) {
		for (i = 0; i < count; i++) {
			if (i != reqpage) {
				swap_pager_freepage(m[i]);
				m[i] = 0;
			}
		}
		count = 1;
		m[0] = m[reqpage];
		reqpage = 0;
	/*
	 * get a swap pager clean data structure, block until we get it
	 */
		if (queue_empty(&swap_pager_free)) {
			s = splbio();
			(void) swap_pager_clean(NULL, B_WRITE);
			while (queue_empty(&swap_pager_free)) { 
				swap_pager_needflags |= SWAP_FREE_NEEDED;
				tsleep((caddr_t)&swap_pager_free,
					PVM, "swpfre", 0);
				(void) swap_pager_clean(NULL, B_WRITE);
			}
			splx(s);
		}
		queue_remove_first(&swap_pager_free, spc, swp_clean_t, spc_list);
		kva = spc->spc_kva;
	}
	

	/*
	 * Determine swap block and allocate as necessary.
	 * We try to get SWB_NPAGES first, but then we punt and try
	 * to get one page.  If that fails, we look at the allocation
	 * data structures to find unused but allocated pages in other
	 * pagers allocations.
	 */
	if (reqaddr == SWB_EMPTY) {
		int blk;
		int tries;
		int ntoget;

		tries = 0;
		s = splbio();
		for (i = 0; i < SWB_NPAGES; i++) {
			if (swb->swb_block[i] != SWB_EMPTY)
				break;
		}

		ntoget = (i == SWB_NPAGES) ? SWB_NPAGES : 1;
retrygetspace:
		if (ntoget == SWB_NPAGES &&
			rlist_alloc(&swapmap, btodb(ntoget * NBPG),&blk)) {
			for (i = 0; i < ntoget; i++)
				swb->swb_block[i] = blk + btodb(NBPG) * i;
		} else if (!rlist_alloc(&swapmap, btodb(NBPG), &swb->swb_block[off])) {
				if (++tries == 1) {
					swap_pager_reclaim();
					goto retrygetspace;
				}
				if (spc)
					queue_enter(&swap_pager_free, spc, swp_clean_t, spc_list);
				if (swap_pager_full == 0) 
					printf("swap_pager: out of swap space !!!\n");
				swap_pager_full = 1;
				swap_pager_ridpages(m, count, reqpage);
				splx(s);
				return(VM_PAGER_TRYAGAIN);
		}
		splx(s);
		swap_pager_full = 0;
	}

	for (i = 0; i < count; i++) {
		pmap_enter(vm_map_pmap(pager_map), kva + NBPG * i,
			VM_PAGE_TO_PHYS(m[i]), VM_PROT_ALL, TRUE);
	}
				
	off = swap_pager_block_offset(swp, m[0]->offset + paging_offset) / NBPG;

#ifdef DEBUG
	if (flags & B_READ && count > 1)
		printf("obj: 0x%x off: 0x%x poff: 0x%x off: 0x%x, sz: %d blk: %d op: %s\n",
			object, m[0]->offset, paging_offset, off, count, swb->swb_block[off], flags&B_READ?"r":"w");
#endif

	s = splbio();
	/*
	 * Get a swap buffer header and perform the IO
	 */
	bp = getpbuf();
	bp->b_flags = B_BUSY | (flags & B_READ);
	bp->b_proc = &proc0;	/* XXX (but without B_PHYS set this is ok) */
	bp->b_rcred = bp->b_wcred = bp->b_proc->p_ucred;
	bp->b_un.b_addr = (caddr_t) kva;
	bp->b_blkno = swb->swb_block[off];
	VHOLD(swapdev_vp);
	bp->b_vp = swapdev_vp;
	if (swapdev_vp->v_type == VBLK)
		bp->b_dev = swapdev_vp->v_rdev;
	bp->b_bcount = NBPG*count;
	if ((bp->b_flags & B_READ) == 0)
		swapdev_vp->v_numoutput++;

	/*
	 * If this is an async write we set up additional buffer fields
	 * and place a "cleaning" entry on the inuse queue.
	 */
	if ((flags & (B_READ|B_ASYNC)) == B_ASYNC) {
		spc->spc_flags = 0;
		spc->spc_bp = bp;
		spc->spc_swp = swp;
		spc->spc_m = m[reqpage];
		bp->b_flags |= B_CALL;
		bp->b_iodone = swap_pager_iodone;
		bp->b_dirtyoff = 0;
		bp->b_dirtyend = bp->b_bcount;
		bp->b_spc = (void *) spc;
		swp->sw_poip++;
		queue_enter(&swap_pager_inuse, spc, swp_clean_t, spc_list);
		swb->swb_valid |= (1 << off);
	} else {
		if ((flags & B_READ) == 0) {
			swb->swb_valid |= (1 << off);
			swp->sw_poip++;
		} else {
			swp->sw_piip++;
		}
		bp->b_flags |= B_CALL;
		bp->b_iodone = swap_pager_iodone1;
	}
	VOP_STRATEGY(bp);
	if ((flags & (B_READ|B_ASYNC)) == B_ASYNC ) {
		if ((bp->b_flags & B_DONE) == B_DONE) {
			swap_pager_clean(NULL, flags);
		}
		splx(s);
		return(VM_PAGER_PEND);
	}
	while ((bp->b_flags & B_DONE) == 0) {
		tsleep((caddr_t)bp, PVM, (flags & B_READ)?"swread":"swwrt", 0);
	}
	rv = (bp->b_flags & B_ERROR) ? VM_PAGER_FAIL : VM_PAGER_OK;
	bp->b_flags &= ~(B_BUSY|B_WANTED|B_PHYS|B_DIRTY|B_CALL|B_DONE);

	if (bp->b_flags & B_READ) {
		--swp->sw_piip;
		if (swp->sw_piip == 0)
			wakeup((caddr_t) swp);
	} else {
		--swp->sw_poip;
		if (swp->sw_poip == 0)
			wakeup((caddr_t) swp);
	}
		
	if (bp->b_vp)
		brelvp(bp);

	relpbuf(bp);

	splx(s);

	pmap_remove(vm_map_pmap(pager_map), kva, kva + count * NBPG);

	if ((flags & B_READ) == 0 && rv == VM_PAGER_OK) {
		m[reqpage]->flags |= PG_CLEAN;
		pmap_clear_modify(VM_PAGE_TO_PHYS(m[reqpage]));
		if (pmap_is_referenced(VM_PAGE_TO_PHYS(m[reqpage])))
			vm_page_activate(m[reqpage]);
		pmap_clear_reference(VM_PAGE_TO_PHYS(m[reqpage]));
	}

	if (spc) {
		queue_enter(&swap_pager_free, spc, swp_clean_t, spc_list);
	} else {
		for (i = 0; i < count; i++) {
			pmap_clear_modify(VM_PAGE_TO_PHYS(m[i]));
			m[i]->flags |= PG_CLEAN;
			m[i]->flags &= ~PG_LAUNDRY;
			if (i != reqpage) {
				/*
				 * whether or not to leave the page activated
				 * is up in the air, but we should put the page
				 * on a page queue somewhere. (it already is in
				 * the object).
				 */
				if (i < (reqpage + 3))
					vm_page_activate(m[i]);
				else
					vm_page_deactivate(m[i]); 

				/*
				 * just in case someone was asking for this
				 * page we now tell them that it is ok to use
				 */
				m[i]->flags &= ~PG_FAKE;
				PAGE_WAKEUP(m[i]);
			}
		}
/*
 * and free the kernel virtual addresses
 */
		kmem_free_wakeup(pager_map, kva, count * NBPG);
	}
	return(rv);
}

boolean_t
swap_pager_clean(m, rw)
	vm_page_t m;
	int rw;
{
	register swp_clean_t spc, tspc;
	register int s;

	tspc = NULL;
	if (queue_empty(&swap_pager_done))
		return FALSE;
	for (;;) {
		s = splbio();
		/*
		 * Look up and removal from done list must be done
		 * at splbio() to avoid conflicts with swap_pager_iodone.
		 */
		spc = (swp_clean_t) queue_first(&swap_pager_done);
		while (!queue_end(&swap_pager_done, (queue_entry_t)spc)) {
			pmap_remove(vm_map_pmap(pager_map), spc->spc_kva, ((vm_offset_t) spc->spc_kva) + NBPG);
			swap_pager_finish(spc);
			queue_remove(&swap_pager_done, spc, swp_clean_t, spc_list);
			goto doclean;
		}

		/*
		 * No operations done, thats all we can do for now.
		 */

		splx(s);
		break;

		/*
		 * The desired page was found to be busy earlier in
		 * the scan but has since completed.
		 */
doclean:
		if (tspc && tspc == spc) {
			tspc = NULL;
		}
		spc->spc_flags = 0;
		queue_enter(&swap_pager_free, spc, swp_clean_t, spc_list);
		++cleandone;
		splx(s);
	}

	return(tspc ? TRUE : FALSE);
}

void
swap_pager_finish(spc)
	register swp_clean_t spc;
{
	vm_page_t m = spc->spc_m;
	vm_object_t object = m->object;
	extern int vm_pageout_free_min;

	if (--object->paging_in_progress == 0) 
		thread_wakeup((int) object);

	/*
	 * If no error mark as clean and inform the pmap system.
	 * If error, mark as dirty so we will try again.
	 * (XXX could get stuck doing this, should give up after awhile)
	 */
	if (spc->spc_flags & SPC_ERROR) {
		printf("swap_pager_finish: clean of page %x failed\n",
		       VM_PAGE_TO_PHYS(m));
		m->flags |= PG_LAUNDRY;
	} else {
		pmap_clear_modify(VM_PAGE_TO_PHYS(m));
		m->flags |= PG_CLEAN;
	}

	if (pmap_is_referenced(VM_PAGE_TO_PHYS(m))) 
		vm_page_activate(m);

	pmap_clear_reference(VM_PAGE_TO_PHYS(m));
	PAGE_WAKEUP(m);
	/*
	 * if we need memory desperately, then free it now
	 */
	if (vm_page_free_count < vm_page_free_reserved &&
		(m->flags & PG_CLEAN) && m->wire_count == 0 &&
		!pmap_is_wired(VM_PAGE_TO_PHYS(m))) {
		pmap_page_protect(VM_PAGE_TO_PHYS(m), VM_PROT_NONE);
		vm_page_free(m);
	}
	--nswiodone;

	return;
}

/*
 * swap_pager_iodone
 */
void
swap_pager_iodone(bp)
	register struct buf *bp;
{
	register swp_clean_t spc;
	daddr_t blk;
	int s;

	s = splbio();
	spc = (swp_clean_t) bp->b_spc;
	queue_remove(&swap_pager_inuse, spc, swp_clean_t, spc_list);
	queue_enter(&swap_pager_done, spc, swp_clean_t, spc_list);
	if (bp->b_flags & B_ERROR) {
		spc->spc_flags |= SPC_ERROR;
		printf("error %d blkno %d sz %d ",
			bp->b_error, bp->b_blkno, bp->b_bcount);
	}
	spc->spc_bp = NULL;

	if ((bp->b_flags & B_READ) == 0)
		vwakeup(bp);
		
	bp->b_flags &= ~(B_BUSY|B_WANTED|B_PHYS|B_DIRTY|B_ASYNC);
	if (bp->b_vp) {
		brelvp(bp);
	}

	relpbuf(bp);
	nswiodone++;
	if (--spc->spc_swp->sw_poip == 0) {
		wakeup((caddr_t)spc->spc_swp);
	}

	if ((swap_pager_needflags & SWAP_FREE_NEEDED) ||
	    queue_empty(&swap_pager_inuse)) { 
		swap_pager_needflags &= ~SWAP_FREE_NEEDED;
		wakeup((caddr_t)&swap_pager_free);
	}

	if (vm_pageout_pages_needed) {
		wakeup((caddr_t)&vm_pageout_pages_needed);
	}

	if (queue_empty(&swap_pager_inuse) ||
	    (vm_page_free_count < vm_page_free_min &&
	    nswiodone + vm_page_free_count >= vm_page_free_min) ) {
		wakeup((caddr_t)&vm_pages_needed);
	}
	splx(s);
}

/*
 * allocate a physical buffer 
 */
struct buf *
getpbuf() {
	int s;
	struct buf *bp;

	s = splbio();
	/* get a bp from the swap buffer header pool */
	while (bswlist.av_forw == NULL) {
		bswlist.b_flags |= B_WANTED;
		tsleep((caddr_t)&bswlist, PVM, "wswbuf", 0); 
	}
	bp = bswlist.av_forw;
	bswlist.av_forw = bp->av_forw;

	splx(s);

	bzero(bp, sizeof *bp);
	return bp;
}

/*
 * release a physical buffer
 */
void
relpbuf(bp)
	struct buf *bp;
{
	int s;

	s = splbio();
	bp->av_forw = bswlist.av_forw;
	bswlist.av_forw = bp;
	if (bswlist.b_flags & B_WANTED) {
		bswlist.b_flags &= ~B_WANTED;
		wakeup((caddr_t)&bswlist);
	}
	splx(s);
}

