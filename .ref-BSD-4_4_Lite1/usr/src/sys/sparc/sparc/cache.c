/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
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
 *	@(#)cache.c	8.2 (Berkeley) 10/30/93
 *
 * from: $Header: cache.c,v 1.12 93/10/31 05:27:47 torek Exp $ (LBL)
 */

/*
 * Cache routines.
 *
 * TODO:
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
	register u_int i, lim, ls, ts;

	ls = cacheinfo.c_linesize;
	ts = cacheinfo.c_totalsize;
	for (i = AC_CACHETAGS, lim = i + ts; i < lim; i += ls)
		sta(i, ASI_CONTROL, 0);

	stba(AC_SYSENABLE, ASI_CONTROL,
	    lduba(AC_SYSENABLE, ASI_CONTROL) | SYSEN_CACHE);
	cacheinfo.c_enabled = 1;

	printf("%d byte (%d/line) write-through %cw flush cache enabled\n",
	    ts, ls, cacheinfo.c_hwflush ? 'h' : 's');
}


/*
 * Flush the current context from the cache.
 *
 * This is done by writing to each cache line in the `flush context'
 * address space (or, for hardware flush, once to each page in the
 * hardware flush space, for all cache pages).
 */
void
cache_flush_context()
{
	register char *p;
	register int i, ls;

	cachestats.cs_ncxflush++;
	p = (char *)0;	/* addresses 0..cacheinfo.c_totalsize will do fine */
	if (cacheinfo.c_hwflush) {
		ls = NBPG;
		i = cacheinfo.c_totalsize >> PGSHIFT;
		for (; --i >= 0; p += ls)
			sta(p, ASI_HWFLUSHCTX, 0);
	} else {
		ls = cacheinfo.c_linesize;
		i = cacheinfo.c_totalsize >> cacheinfo.c_l2linesize;
		for (; --i >= 0; p += ls)
			sta(p, ASI_FLUSHCTX, 0);
	}
}

/*
 * Flush the given virtual segment from the cache.
 *
 * This is also done by writing to each cache line, except that
 * now the addresses must include the virtual segment number, and
 * we use the `flush segment' space.
 *
 * Again, for hardware, we just write each page (in hw-flush space).
 */
void
cache_flush_segment(vseg)
	register int vseg;
{
	register int i, ls;
	register char *p;

	cachestats.cs_nsgflush++;
	p = (char *)VSTOVA(vseg);	/* seg..seg+sz rather than 0..sz */
	if (cacheinfo.c_hwflush) {
		ls = NBPG;
		i = cacheinfo.c_totalsize >> PGSHIFT;
		for (; --i >= 0; p += ls)
			sta(p, ASI_HWFLUSHSEG, 0);
	} else {
		ls = cacheinfo.c_linesize;
		i = cacheinfo.c_totalsize >> cacheinfo.c_l2linesize;
		for (; --i >= 0; p += ls)
			sta(p, ASI_FLUSHSEG, 0);
	}
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

	if (i <= 15) {
		/* cache_flush_page, for i pages */
		p = (char *)((int)base & ~baseoff);
		if (cacheinfo.c_hwflush) {
			for (; --i >= 0; p += NBPG)
				sta(p, ASI_HWFLUSHPG, 0);
		} else {
			ls = cacheinfo.c_linesize;
			i <<= PGSHIFT - cacheinfo.c_l2linesize;
			for (; --i >= 0; p += ls)
				sta(p, ASI_FLUSHPG, 0);
		}
		return;
	}
	baseoff = (u_int)base & SGOFSET;
	i = (baseoff + len + SGOFSET) >> SGSHIFT;
	if (i == 1)
		cache_flush_segment(VA_VSEG(base));
	else
		cache_flush_context();
}
