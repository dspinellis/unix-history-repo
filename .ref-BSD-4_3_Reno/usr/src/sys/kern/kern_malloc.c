/*
 * Copyright (c) 1987 Regents of the University of California.
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
 *	@(#)kern_malloc.c	7.19 (Berkeley) 7/27/90
 */

#include "param.h"
#include "vm.h"
#include "cmap.h"
#include "time.h"
#include "proc.h"
#include "map.h"
#include "kernel.h"
#include "malloc.h"

#include "machine/pte.h"

struct kmembuckets bucket[MINBUCKET + 16];
struct kmemstats kmemstats[M_LAST];
struct kmemusage *kmemusage;
long wantkmemmap;

/*
 * Allocate a block of memory
 */
qaddr_t
malloc(size, type, flags)
	unsigned long size;
	int type, flags;
{
	register struct kmembuckets *kbp;
	register struct kmemusage *kup;
	long indx, npg, alloc, allocsize;
	int s;
	caddr_t va, cp;
#ifdef KMEMSTATS
	register struct kmemstats *ksp = &kmemstats[type];

	if (((unsigned long)type) > M_LAST)
		panic("malloc - bogus type");
#endif

	indx = BUCKETINDX(size);
	kbp = &bucket[indx];
	s = splimp();
again:
#ifdef KMEMSTATS
	while (ksp->ks_memuse >= ksp->ks_limit) {
		if (flags & M_NOWAIT) {
			splx(s);
			return (0);
		}
		if (ksp->ks_limblocks < 65535)
			ksp->ks_limblocks++;
		sleep((caddr_t)ksp, PSWP+2);
	}
#endif
	if (kbp->kb_next == NULL) {
		if (size > MAXALLOCSAVE)
			allocsize = roundup(size, CLBYTES);
		else
			allocsize = 1 << indx;
		npg = clrnd(btoc(allocsize));
		if ((flags & M_NOWAIT) && freemem < npg) {
			splx(s);
			return (0);
		}
		alloc = rmalloc(kmemmap, npg);
		if (alloc == 0) {
			if (flags & M_NOWAIT) {
				splx(s);
				return (0);
			}
#ifdef KMEMSTATS
			if (ksp->ks_mapblocks < 65535)
				ksp->ks_mapblocks++;
#endif
			wantkmemmap++;
			sleep((caddr_t)&wantkmemmap, PSWP+2);
			goto again;
		}
		alloc -= CLSIZE;		/* convert to base 0 */
		(void) vmemall(&kmempt[alloc], (int)npg, &proc[0], CSYS);
		va = (caddr_t) kmemxtob(alloc);
		vmaccess(&kmempt[alloc], va, (int)npg);
#ifdef KMEMSTATS
		kbp->kb_total += kbp->kb_elmpercl;
#endif
		kup = btokup(va);
		kup->ku_indx = indx;
		if (allocsize > MAXALLOCSAVE) {
			if (npg > 65535)
				panic("malloc: allocation too large");
			kup->ku_pagecnt = npg;
#ifdef KMEMSTATS
			ksp->ks_memuse += allocsize;
#endif
			goto out;
		}
#ifdef KMEMSTATS
		kup->ku_freecnt = kbp->kb_elmpercl;
		kbp->kb_totalfree += kbp->kb_elmpercl;
#endif
		kbp->kb_next = va + (npg * NBPG) - allocsize;
		for (cp = kbp->kb_next; cp > va; cp -= allocsize)
			*(caddr_t *)cp = cp - allocsize;
		*(caddr_t *)cp = NULL;
	}
	va = kbp->kb_next;
	kbp->kb_next = *(caddr_t *)va;
#ifdef KMEMSTATS
	kup = btokup(va);
	if (kup->ku_indx != indx)
		panic("malloc: wrong bucket");
	if (kup->ku_freecnt == 0)
		panic("malloc: lost data");
	kup->ku_freecnt--;
	kbp->kb_totalfree--;
	ksp->ks_memuse += 1 << indx;
out:
	kbp->kb_calls++;
	ksp->ks_inuse++;
	ksp->ks_calls++;
	if (ksp->ks_memuse > ksp->ks_maxused)
		ksp->ks_maxused = ksp->ks_memuse;
#else
out:
#endif
	splx(s);
	return ((qaddr_t)va);
}

#ifdef DIAGNOSTIC
long addrmask[] = { 0x00000000,
	0x00000001, 0x00000003, 0x00000007, 0x0000000f,
	0x0000001f, 0x0000003f, 0x0000007f, 0x000000ff,
	0x000001ff, 0x000003ff, 0x000007ff, 0x00000fff,
	0x00001fff, 0x00003fff, 0x00007fff, 0x0000ffff,
};
#endif /* DIAGNOSTIC */

/*
 * Free a block of memory allocated by malloc.
 */
void
free(addr, type)
	caddr_t addr;
	int type;
{
	register struct kmembuckets *kbp;
	register struct kmemusage *kup;
	long alloc, size;
	int s;
#ifdef KMEMSTATS
	register struct kmemstats *ksp = &kmemstats[type];
#endif

	kup = btokup(addr);
	size = 1 << kup->ku_indx;
#ifdef DIAGNOSTIC
	if (size > NBPG * CLSIZE)
		alloc = addrmask[BUCKETINDX(NBPG * CLSIZE)];
	else
		alloc = addrmask[kup->ku_indx];
	if (((u_long)addr & alloc) != 0) {
		printf("free: unaligned addr 0x%x, size %d, type %d, mask %d\n",
			addr, size, type, alloc);
		panic("free: unaligned addr");
	}
#endif /* DIAGNOSTIC */
	kbp = &bucket[kup->ku_indx];
	s = splimp();
	if (size > MAXALLOCSAVE) {
		alloc = btokmemx(addr);
		(void) memfree(&kmempt[alloc], (int)kup->ku_pagecnt, 1);
		rmfree(kmemmap, (long)kup->ku_pagecnt, alloc + CLSIZE);
		if (wantkmemmap) {
			wakeup((caddr_t)&wantkmemmap);
			wantkmemmap = 0;
		}
#ifdef KMEMSTATS
		size = kup->ku_pagecnt << PGSHIFT;
		ksp->ks_memuse -= size;
		kup->ku_indx = 0;
		kup->ku_pagecnt = 0;
		if (ksp->ks_memuse + size >= ksp->ks_limit &&
		    ksp->ks_memuse < ksp->ks_limit)
			wakeup((caddr_t)ksp);
		ksp->ks_inuse--;
		kbp->kb_total -= 1;
#endif
		splx(s);
		return;
	}
#ifdef KMEMSTATS
	kup->ku_freecnt++;
	if (kup->ku_freecnt >= kbp->kb_elmpercl)
		if (kup->ku_freecnt > kbp->kb_elmpercl)
			panic("free: multiple frees");
		else if (kbp->kb_totalfree > kbp->kb_highwat)
			kbp->kb_couldfree++;
	kbp->kb_totalfree++;
	ksp->ks_memuse -= size;
	if (ksp->ks_memuse + size >= ksp->ks_limit &&
	    ksp->ks_memuse < ksp->ks_limit)
		wakeup((caddr_t)ksp);
	ksp->ks_inuse--;
#endif
	*(caddr_t *)addr = kbp->kb_next;
	kbp->kb_next = addr;
	splx(s);
}

/*
 * Initialize the kernel memory allocator
 */
kmeminit()
{
	register long indx;
	int npg;

#if	((MAXALLOCSAVE & (MAXALLOCSAVE - 1)) != 0)
		ERROR!_kmeminit:_MAXALLOCSAVE_not_power_of_2
#endif
#if	(MAXALLOCSAVE > MINALLOCSIZE * 32768)
		ERROR!_kmeminit:_MAXALLOCSAVE_too_big
#endif
#if	(MAXALLOCSAVE < CLBYTES)
		ERROR!_kmeminit:_MAXALLOCSAVE_too_small
#endif
	npg = ekmempt - kmempt;
	rminit(kmemmap, (long)npg, (long)CLSIZE, "malloc map", npg);
#ifdef KMEMSTATS
	for (indx = 0; indx < MINBUCKET + 16; indx++) {
		if (1 << indx >= CLBYTES)
			bucket[indx].kb_elmpercl = 1;
		else
			bucket[indx].kb_elmpercl = CLBYTES / (1 << indx);
		bucket[indx].kb_highwat = 5 * bucket[indx].kb_elmpercl;
	}
	for (indx = 0; indx < M_LAST; indx++)
		kmemstats[indx].ks_limit = npg * NBPG * 6 / 10;
#endif
}
