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
 *	California, Lawrence Berkeley Laboratories.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)cache.c	7.3 (Berkeley) %G%
 *
 * from: $Header: cache.c,v 1.5 92/06/17 05:21:56 torek Exp $ (LBL)
 */

/*
 * Cache routines.
 */

#include <sys/param.h>

#include <machine/pte.h>

#include <sparc/sparc/asm.h>
#include <sparc/sparc/cache.h>
#include <sparc/sparc/ctlreg.h>

enum vactype vactype;

/*
 * Enable the cache.
 * We need to clear out the valid bits first.
 */
void
cache_enable()
{
	register int i;

	for (i = AC_CACHETAGS; i < AC_CACHETAGS + 4096 * 4; i += 4)
		sta(i, ASI_CONTROL, 0);

	vactype = VAC_WRITETHROUGH;		/* XXX must be set per cpu */
	stba(AC_SYSENABLE, ASI_CONTROL,
	    lduba(AC_SYSENABLE, ASI_CONTROL) | SYSEN_CACHE);
	printf("cache enabled\n");
}


/*
 * Flush the current context from the cache.
 *
 * This is done by writing to each cache line in the `flush context'
 * address space.  Cache lines are 16 bytes, hence the declaration of `p'.
 */
void
cache_flush_context()
{
	register int i;
	register char (*p)[16];

	p = 0;
	for (i = 0x1000; --i >= 0; p++)
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
	register int i;
	register char (*p)[16];

	p = (char (*)[16])VSTOVA(vseg);
	for (i = 0x1000; --i >= 0; p++)
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
	register int i;
	register char (*p)[16];

	p = (char (*)[16])va;
	for (i = NBPG >> 4; --i >= 0; p++)
		sta(p, ASI_FLUSHPG, 0);
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
	register int i, baseoff;
	register char (*p)[16];

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
	 */
	baseoff = (int)base & PGOFSET;
	i = (baseoff + len + PGOFSET) >> PGSHIFT;
	if (i <= 15) {
		/* cache_flush_page, for i pages */
		p = (char (*)[16])((int)base & ~baseoff);
		for (i <<= PGSHIFT - 4; --i >= 0; p++)
			sta(p, ASI_FLUSHPG, 0);
		return;
	}
	baseoff = (u_int)base & SGOFSET;
	i = (baseoff + len + SGOFSET) >> SGSHIFT;
	if (i == 1) {
		/* cache_flush_segment */
		p = (char (*)[16])((int)base & ~baseoff);
		for (i = 0x1000; --i >= 0; p++)
			sta(p, ASI_FLUSHSEG, 0);
		return;
	}
	/* cache_flush_context */
	p = 0;
	for (i = 0x1000; --i >= 0; p++)
		sta(p, ASI_FLUSHCTX, 0);
}
