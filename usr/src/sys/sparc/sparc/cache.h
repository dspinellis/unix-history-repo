/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)cache.h	7.1 (Berkeley) %G%
 *
 * from: $Header: cache.h,v 1.5 92/06/17 05:21:57 torek Exp $
 */

/*
 * Sun-4 and Sun-4c virtual address cache.
 *
 * Sun-4 virtual caches come in two flavors, write-through (Sun-4c)
 * and write-back (Sun-4).  The write-back caches are much faster
 * but require a bit more care.
 *
 * VAC_NONE is not actually used now, but if someone builds a physical
 * cache Sun-4 (or, more likely, a virtual index/physical tag cache)
 * everything will work (after pulling out the #ifdef notdef's: grep
 * for VAC_NONE to find them).
 */
enum vactype { VAC_NONE, VAC_WRITETHROUGH, VAC_WRITEBACK };

extern enum vactype vactype;

/*
 * Cache tags can be written in control space, and must be set to 0
 * (or invalid anyway) before turning on the cache.  The tags are
 * addressed as an array of 32-bit structures of the form:
 *
 *	struct cache_tag {
 *		u_int	:7,		(unused; must be zero)
 *			ct_cid:3,	(context ID)
 *			ct_w:1,		(write flag from PTE)
 *			ct_s:1,		(supervisor flag from PTE)
 *			ct_v:1,		(set => cache entry is valid)
 *			:3,		(unused; must be zero)
 *			ct_tid:14,	(cache tag ID)
 *			:2;		(unused; must be zero)
 *	};
 *
 * The cache sees virtual addresses as:
 *
 *	struct cache_va {
 *		u_int	:2,		(unused; probably copies of va_tid<13>)
 *			cva_tid:14,	(tag ID)
 *			cva_line:12,	(cache line number)
 *			cva_byte:4;	(byte in cache line)
 *	};
 *
 * Note that, because the 12-bit line ID is `wider' than the page offset,
 * it is possible to have one page map to two different cache lines.
 * This can happen whenever two different physical pages have the same bits
 * in the part of the virtual address that overlaps the cache line ID, i.e.,
 * bits <15:12>.  In order to prevent cache duplication, we have to
 * make sure that no one page has more than one virtual address where
 * (va1 & 0xf000) != (va2 & 0xf000).  (The cache hardware turns off ct_v
 * when a cache miss occurs on a write, i.e., if va1 is in the cache and
 * va2 is not, and you write to va2, va1 goes out of the cache.  If va1
 * is in the cache and va2 is not, reading va2 also causes va1 to become
 * uncached, and the [same] data is then read from main memory into the
 * cache.)
 *
 * The other alternative, of course, is to disable caching of aliased
 * pages.  (In a few cases this might be faster anyway, but we do it
 * only when forced.)
 *
 * THE CURRENT VM CODE DOES NOT ALLOW US TO SPECIFY PREFERRED VIRTUAL
 * ADDRESSES ... THIS MUST BE FIXED!
 */

#define	CACHE_ALIAS_DISTANCE	(256 * 1024)	/* 256 kbytes */

/*
 * True iff a1 and a2 are `bad' aliases (will cause cache duplication).
 */
#define	BADALIAS(a1, a2) (((int)(a1) ^ (int)(a2)) & 0xf000)

/*
 * Routines for dealing with the cache.
 */
void	cache_enable __P((void));		/* turn it on */
void	cache_flush_context __P((void));	/* flush current context */
void	cache_flush_segment __P((int vseg));	/* flush seg in cur ctx */
void	cache_flush_page __P((int va));		/* flush page in cur ctx */
void	cache_flush __P((caddr_t base, u_int len));/* flush region */
