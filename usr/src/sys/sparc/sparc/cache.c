/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Lawrence Berkeley Laboratory.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)cache.c	7.5 (Berkeley) %G%
 *
 * from: $Header: cache.c,v 1.8 93/04/27 14:33:36 torek Exp $ (LBL)
 */

/*
 * Cache routines.
 *
 * TODO:
 *	- fill in hardware assist for context and segment flush
 *	- rework range flush
 */

#include <sys/param.h>

#include <machine/ctlreg.h>
#include <machine/pte.h>

#include <sparc/sparc/asm.h>
#include <sparc/sparc/cache.h>

enum vactype vactype;
struct cachestats cachestats;

/*
 * Enable the cache.
 * We need to clear out the valid bits first.
 */
void
cache_enable()
{
	register int i, lim, ls;

	i = AC_CACHETAGS;
	lim = i + cacheinfo.c_totalsize;
	ls = cacheinfo.c_linesize;
	for (; i < lim; i += ls)
		sta(i, ASI_CONTROL, 0);

	stba(AC_SYSENABLE, ASI_CONTROL,
	    lduba(AC_SYSENABLE, ASI_CONTROL) | SYSEN_CACHE);
	cacheinfo.c_enabled = 1;
	printf("cache enabled\n");
}


/*
 * Flush the current context from the cache.
 *
 * This is done by writing to each cache line in the `flush context'
 * address space.
 */
void
cache_flush_context()
{
	register int i, ls;
	register char *p;

	cachestats.cs_ncxflush++;
	ls = cacheinfo.c_linesize;
	i = cacheinfo.c_totalsize >> cacheinfo.c_l2linesize;
	for (p = 0; --i >= 0; p += ls)
		sta(p, ASI_FLUSHCTX, 0);
}

/*
 * Flush the given virtual segment from the cache.
 *
 * This is also done by writing to each cache line, except that
 * now the addresses must include the virtual segment number, and
 * we use the `flush segment' space.
 */
void
cache_flush_segment(vseg)
	register int vseg;
{
	register int i, ls;
	register char *p;

	cachestats.cs_nsgflush++;
	ls = cacheinfo.c_linesize;
	i = cacheinfo.c_totalsize >> cacheinfo.c_l2linesize;
	for (p = (char *)VSTOVA(vseg); --i >= 0; p += ls)
		sta(p, ASI_FLUSHSEG, 0);
}

/*
 * Flush the given virtual page from the cache.
 * (va is the actual address, and must be aligned on a page boundary.)
 * Again we write to each cache line.
 */
void
cache_flush_page(va)
	int va;
{
	register int i, ls;
	register char *p;

	cachestats.cs_npgflush++;
	p = (char *)va;
	if (cacheinfo.c_hwflush)
		sta(p, ASI_HWFLUSHPG, 0);
	else {
		ls = cacheinfo.c_linesize;
		i = NBPG >> cacheinfo.c_l2linesize;
		for (; --i >= 0; p += ls)
			sta(p, ASI_FLUSHPG, 0);
	}
}

/*
 * Flush a range of virtual addresses (in the current context).
 * The first byte is at (base&~PGOFSET) and the last one is just
 * before byte (base+len).
 *
 * We choose the best of (context,segment,page) here.
 */
void
cache_flush(base, len)
	caddr_t base;
	register u_int len;
{
	register int i, ls, baseoff;
	register char *p;

	/*
	 * Figure out how much must be flushed.
	 *
	 * If we need to do 16 pages, we can do a segment in the same
	 * number of loop iterations.  We can also do the context.  If
	 * we would need to do two segments, do the whole context.
	 * This might not be ideal (e.g., fsck likes to do 65536-byte
	 * reads, which might not necessarily be aligned).
	 *
	 * We could try to be sneaky here and use the direct mapping
	 * to avoid flushing things `below' the start and `above' the
	 * ending address (rather than rounding to whole pages and
	 * segments), but I did not want to debug that now and it is
	 * not clear it would help much.
	 *
	 * (XXX the magic number 16 is now wrong, must review policy)
	 */
	baseoff = (int)base & PGOFSET;
	i = (baseoff + len + PGOFSET) >> PGSHIFT;

	cachestats.cs_nraflush++;
#ifdef notyet
	cachestats.cs_ra[min(i, MAXCACHERANGE)]++;
#endif

	ls = cacheinfo.c_linesize;
	if (i <= 15) {
		/* cache_flush_page, for i pages */
		p = (char *)((int)base & ~baseoff);
		if (cacheinfo.c_hwflush) {
			for (; --i >= 0; p += NBPG)
				sta(p, ASI_HWFLUSHPG, 0);
		} else {
			i <<= PGSHIFT - cacheinfo.c_l2linesize;
			for (; --i >= 0; p += ls)
				sta(p, ASI_FLUSHPG, 0);
		}
		return;
	}
	baseoff = (u_int)base & SGOFSET;
	i = (baseoff + len + SGOFSET) >> SGSHIFT;
	if (i == 1) {
		/* cache_flush_segment */
		p = (char *)((int)base & ~baseoff);
		i = cacheinfo.c_totalsize >> cacheinfo.c_l2linesize;
		for (; --i >= 0; p += ls)
			sta(p, ASI_FLUSHSEG, 0);
		return;
	}
	/* cache_flush_context */
	i = cacheinfo.c_totalsize >> cacheinfo.c_l2linesize;
	for (p = 0; --i >= 0; p += ls)
		sta(p, ASI_FLUSHCTX, 0);
}
