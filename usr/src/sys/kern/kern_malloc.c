/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)kern_malloc.c	7.5 (Berkeley) %G%
 */

#include "param.h"
#include "vm.h"
#include "cmap.h"
#include "time.h"
#include "proc.h"
#include "map.h"
#include "kernel.h"
#include "malloc.h"

#include "../machine/pte.h"

struct kmembuckets bucket[MINBUCKET + 16];
struct kmemstats kmemstats[M_LAST];
struct kmemusage *kmemusage;
long wantkmemmap;

/*
 * Allocate a block of memory
 */
qaddr_t malloc(size, type, flags)
	unsigned long size;
	long type, flags;
{
	register struct kmembuckets *kbp;
	register struct kmemusage *kup;
	long indx, npg, alloc, allocsize, s;
	caddr_t va, cp;
#ifdef KMEMSTATS
	register struct kmemstats *ksp = &kmemstats[type];
#endif

	indx = BUCKETINDX(size);
	kbp = &bucket[indx];
	s = splimp();
again:
#ifdef KMEMSTATS
	while (ksp->ks_inuse >= ksp->ks_limit) {
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
		(void) vmemall(&kmempt[alloc], npg, &proc[0], CSYS);
		va = (caddr_t) kmemxtob(alloc);
		vmaccess(&kmempt[alloc], va, npg);
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
	if (ksp->ks_inuse > ksp->ks_maxused)
		ksp->ks_maxused = ksp->ks_inuse;
#else
out:
#endif
	splx(s);
	return ((qaddr_t)va);
}

/*
 * Free a block of memory allocated by malloc.
 */
void free(addr, type)
	caddr_t addr;
	long type;
{
	register struct kmembuckets *kbp;
	register struct kmemusage *kup;
	long alloc, s;
#ifdef KMEMSTATS
	register struct kmemstats *ksp = &kmemstats[type];
#endif

	kup = btokup(addr);
	s = splimp();
	if (1 << kup->ku_indx > MAXALLOCSAVE) {
		alloc = btokmemx(addr);
		(void) memfree(&kmempt[alloc], kup->ku_pagecnt, 0);
		rmfree(kmemmap, (long)kup->ku_pagecnt, alloc + CLSIZE);
		if (wantkmemmap) {
			wakeup((caddr_t)&wantkmemmap);
			wantkmemmap = 0;
		}
#ifdef KMEMSTATS
		ksp->ks_memuse -= kup->ku_pagecnt << PGSHIFT;
		kup->ku_indx = 0;
		kup->ku_pagecnt = 0;
		if (ksp->ks_inuse == ksp->ks_limit)
			wakeup((caddr_t)ksp);
		ksp->ks_inuse--;
#endif
		splx(s);
		return;
	}
	kbp = &bucket[kup->ku_indx];
#ifdef KMEMSTATS
	kup->ku_freecnt++;
	if (kup->ku_freecnt >= kbp->kb_elmpercl)
		if (kup->ku_freecnt > kbp->kb_elmpercl)
			panic("free: multiple frees");
		else if (kbp->kb_totalfree > kbp->kb_highwat)
			kbp->kb_couldfree++;
	kbp->kb_totalfree++;
	if (ksp->ks_inuse == ksp->ks_limit)
		wakeup((caddr_t)ksp);
	ksp->ks_inuse--;
	ksp->ks_memuse -= 1 << kup->ku_indx;
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

	if (!powerof2(MAXALLOCSAVE))
		panic("kmeminit: MAXALLOCSAVE not power of 2");
	if (MAXALLOCSAVE > MINALLOCSIZE * 32768)
		panic("kmeminit: MAXALLOCSAVE too big");
	if (MAXALLOCSAVE < CLBYTES)
		panic("kmeminit: MAXALLOCSAVE too small");
	rminit(kmemmap, ekmempt - kmempt, (long)CLSIZE,
		"malloc map", ekmempt - kmempt);
#ifdef KMEMSTATS
	for (indx = 0; indx < MINBUCKET + 16; indx++) {
		if (1 << indx >= CLBYTES)
			bucket[indx].kb_elmpercl = 1;
		else
			bucket[indx].kb_elmpercl = CLBYTES / (1 << indx);
		bucket[indx].kb_highwat = 5 * bucket[indx].kb_elmpercl;
	}
	for (indx = 0; indx < M_LAST; indx++)
		kmemstats[indx].ks_limit =
			(ekmempt - kmempt) * CLBYTES * 9 / 10;
#endif
}
