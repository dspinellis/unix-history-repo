/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to the Computer Systems
 * Engineering Group at Lawrence Berkeley Laboratory and to the University
 * of California at Berkeley by Jef Poskanzer.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)raster_op.c	7.2 (Berkeley) %G%
 *
 * from: $Header: raster_op.c,v 1.22 92/06/17 08:14:44 torek Exp $
 */

/*
 * Bitblit routine for raster library.
 *
 * This raster-op is machined to exacting tolerances by skilled native
 * craftsmen with pride in their work.
 *
 * The various cases are broken down like this:
 *
 *   src required
 *       1-bit to 1-bit
 *       1-bit to 8-bits
 *       8-bits to 8-bits
 *   no src required
 *       1-bit no-src
 *       8-bits no-src
 */

#include <sys/types.h>
#include <sparc/rcons/raster.h>

/* CONFIGURE: To save on executable size, you can configure out the seldom-used
** logical operations.  With this variable set, the only operations implemented
** are: RAS_SRC, RAS_CLEAR, RAS_SET, RAS_INVERT, RAS_XOR, RAS_INVERTSRC.
*/
#ifdef KERNEL
#define PARTIAL_LOGICAL_OPS
#endif

/* CONFIGURE: bcopy() is supposed to be the ultimately fastest way to move
** bytes, overlapping or not, ignoring the startup cost.  Unfortunately
** this is not true on some systems.  For example, on a Sun 3 running
** SunOS 3.5, bcopy() is about five times slower than a simple for loop
** on overlapping copies.  And on a 4.1.1 SPARC, bcopy() is about 2/3rds
** as fast on backwards overlaps.  So, only define this if your bcopy is ok.
*/
#undef BCOPY_FASTER

/* End of configurable definitions. */


/* Definitions. */

/* Raster-op macros.  These encapsulate the switch statements and so make
** the source code 16 times smaller.  The pre and pst args are code
** fragments to put before and after the assignment in each case.  They
** can be the beginning and end of a loop.  If the pst fragment includes a
** masked assignment, for example to handle the left or right edge cases,
** a good optimizing compiler will simplify the boolean expressions very
** nicely - both cc and gcc on the SPARC will do this.
*/

#ifndef PARTIAL_LOGICAL_OPS

#define ROP_DST(op,pre,d,pst) \
    switch ( op ) \
	{ \
	case RAS_CLEAR: \
	pre \
	(d) = 0; \
	pst \
	break; \
	case RAS_INVERT: \
	pre \
	(d) = ~(d); \
	pst \
	break; \
	case RAS_DST: \
	/* noop */ \
	break; \
	case RAS_SET: \
	pre \
	(d) = ~0; \
	pst \
	break; \
	default: \
	return -1; \
	}

#define ROP_DSTCOLOR(op,pre,d,c,pst) \
    switch ( op ) \
	{ \
	case RAS_CLEAR: \
	pre \
	(d) = 0; \
	pst \
	break; \
	case RAS_INVERT: \
	pre \
	(d) = ~(d); \
	pst \
	break; \
	case RAS_DST: \
	/* noop */ \
	break; \
	case RAS_SET: \
	pre \
	(d) = (c); \
	pst \
	break; \
	default: \
	return -1; \
	}

#define ROP_SRCDST(op,pre,s,d,pst) \
    switch ( op ) \
	{ \
	case RAS_NOTOR: \
	pre \
	(d) = ~( (s) | (d) ); \
	pst \
	break; \
	case RAS_NOTSRC_AND_DST: \
	pre \
	(d) = ~(s) & (d); \
	pst \
	break; \
	case RAS_INVERTSRC: \
	pre \
	(d) = ~(s); \
	pst \
	break; \
	case RAS_SRC_AND_NOTDST: \
	pre \
	(d) = (s) & ~(d); \
	pst \
	break; \
	case RAS_XOR: \
	pre \
	(d) = (s) ^ (d); \
	pst \
	break; \
	case RAS_NOTAND: \
	pre \
	(d) = ~( (s) & (d) ); \
	pst \
	break; \
	case RAS_AND: \
	pre \
	(d) = (s) & (d); \
	pst \
	break; \
	case RAS_NOTXOR: \
	pre \
	(d) = ~( (s) ^ (d) ); \
	pst \
	break; \
	case RAS_NOTSRC_OR_DST: \
	pre \
	(d) = ~(s) | (d); \
	pst \
	break; \
	case RAS_SRC: \
	pre \
	(d) = (s); \
	pst \
	break; \
	case RAS_SRC_OR_NOTDST: \
	pre \
	(d) = (s) | ~(d); \
	pst \
	break; \
	case RAS_OR: \
	pre \
	(d) = (s) | (d); \
	pst \
	break; \
	default: \
	return -1; \
	}

#define ROP_SRCDSTCOLOR(op,pre,s,d,c,pst) \
    switch ( op ) \
	{ \
	case RAS_NOTOR: \
	pre \
	if ( s ) \
	    (d) = ~( (c) | (d) ); \
	else \
	    (d) = ~(d); \
	pst \
	break; \
	case RAS_NOTSRC_AND_DST: \
	pre \
	if ( s ) \
	    (d) = ~(c) & (d); \
	pst \
	break; \
	case RAS_INVERTSRC: \
	pre \
	if ( s ) \
	    (d) = ~(c); \
	else \
	    (d) = ~0; \
	pst \
	break; \
	case RAS_SRC_AND_NOTDST: \
	pre \
	if ( s ) \
	    (d) = (c) & ~(d); \
	else \
	    (d) = 0; \
	pst \
	break; \
	case RAS_XOR: \
	pre \
	if ( s ) \
	    (d) = (c) ^ (d); \
	pst \
	break; \
	case RAS_NOTAND: \
	pre \
	if ( s ) \
	    (d) = ~( (c) & (d) ); \
	else \
	    (d) = ~0; \
	pst \
	break; \
	case RAS_AND: \
	pre \
	if ( s ) \
	    (d) = (c) & (d); \
	else \
	    (d) = 0; \
	pst \
	break; \
	case RAS_NOTXOR: \
	pre \
	if ( s ) \
	    (d) = ~( (c) ^ (d) ); \
	else \
	    (d) = ~(d); \
	pst \
	break; \
	case RAS_NOTSRC_OR_DST: \
	pre \
	if ( s ) \
	    (d) = ~(c) | (d); \
	else \
	    (d) = ~0; \
	pst \
	break; \
	case RAS_SRC: \
	pre \
	if ( s ) \
	    (d) = (c); \
	else \
	    (d) = 0; \
	pst \
	break; \
	case RAS_SRC_OR_NOTDST: \
	pre \
	if ( s ) \
	    (d) = (c) | ~(d); \
	else \
	    (d) = ~(d); \
	pst \
	break; \
	case RAS_OR: \
	pre \
	if ( s ) \
	    (d) = (c) | (d); \
	pst \
	break; \
	default: \
	return -1; \
	}

#else /*PARTIAL_LOGICAL_OPS*/

#define ROP_DST(op,pre,d,pst) \
    switch ( op ) \
	{ \
	case RAS_CLEAR: \
	pre \
	(d) = 0; \
	pst \
	break; \
	case RAS_INVERT: \
	pre \
	(d) = ~(d); \
	pst \
	break; \
	case RAS_SET: \
	pre \
	(d) = ~0; \
	pst \
	break; \
	default: \
	return -1; \
	}

#define ROP_DSTCOLOR(op,pre,d,c,pst) \
    switch ( op ) \
	{ \
	case RAS_CLEAR: \
	pre \
	(d) = 0; \
	pst \
	break; \
	case RAS_INVERT: \
	pre \
	(d) = ~(d); \
	pst \
	break; \
	case RAS_SET: \
	pre \
	(d) = (c); \
	pst \
	break; \
	default: \
	return -1; \
	}

#define ROP_SRCDST(op,pre,s,d,pst) \
    switch ( op ) \
	{ \
	case RAS_INVERTSRC: \
	pre \
	(d) = ~(s); \
	pst \
	break; \
	case RAS_XOR: \
	pre \
	(d) = (s) ^ (d); \
	pst \
	break; \
	case RAS_SRC: \
	pre \
	(d) = (s); \
	pst \
	break; \
	default: \
	return -1; \
	}

#define ROP_SRCDSTCOLOR(op,pre,s,d,c,pst) \
    switch ( op ) \
	{ \
	case RAS_INVERTSRC: \
	pre \
	if ( s ) \
	    (d) = ~(c); \
	else \
	    (d) = ~0; \
	pst \
	break; \
	case RAS_XOR: \
	pre \
	if ( s ) \
	    (d) = (c) ^ (d); \
	pst \
	break; \
	case RAS_SRC: \
	pre \
	if ( s ) \
	    (d) = (c); \
	else \
	    (d) = 0; \
	pst \
	break; \
	default: \
	return -1; \
	}

#endif /*PARTIAL_LOGICAL_OPS*/


/* Variables. */

static int needsrc[16] = { 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0 };
/*                       CLEAR          INVERT          DST            SET */

#ifdef MSBIT_FIRST

u_long raster_bitmask[32] = {
    0x80000000, 0x40000000, 0x20000000, 0x10000000,
    0x08000000, 0x04000000, 0x02000000, 0x01000000,
    0x00800000, 0x00400000, 0x00200000, 0x00100000,
    0x00080000, 0x00040000, 0x00020000, 0x00010000,
    0x00008000, 0x00004000, 0x00002000, 0x00001000,
    0x00000800, 0x00000400, 0x00000200, 0x00000100,
    0x00000080, 0x00000040, 0x00000020, 0x00000010,
    0x00000008, 0x00000004, 0x00000002, 0x00000001 };

#ifdef MSBYTE_FIRST
static u_long leftmask[32] = {
    0x00000000, 0x80000000, 0xc0000000, 0xe0000000,
    0xf0000000, 0xf8000000, 0xfc000000, 0xfe000000,
    0xff000000, 0xff800000, 0xffc00000, 0xffe00000,
    0xfff00000, 0xfff80000, 0xfffc0000, 0xfffe0000,
    0xffff0000, 0xffff8000, 0xffffc000, 0xffffe000,
    0xfffff000, 0xfffff800, 0xfffffc00, 0xfffffe00,
    0xffffff00, 0xffffff80, 0xffffffc0, 0xffffffe0,
    0xfffffff0, 0xfffffff8, 0xfffffffc, 0xfffffffe };
static u_long rightmask[32] = {
    0x00000000, 0x00000001, 0x00000003, 0x00000007,
    0x0000000f, 0x0000001f, 0x0000003f, 0x0000007f,
    0x000000ff, 0x000001ff, 0x000003ff, 0x000007ff,
    0x00000fff, 0x00001fff, 0x00003fff, 0x00007fff,
    0x0000ffff, 0x0001ffff, 0x0003ffff, 0x0007ffff,
    0x000fffff, 0x001fffff, 0x003fffff, 0x007fffff,
    0x00ffffff, 0x01ffffff, 0x03ffffff, 0x07ffffff,
    0x0fffffff, 0x1fffffff, 0x3fffffff, 0x7fffffff };
#endif /*MSBYTE_FIRST*/

#else /*MSBIT_FIRST*/

u_long raster_bitmask[32] = {
    0x00000001, 0x00000002, 0x00000004, 0x00000008,
    0x00000010, 0x00000020, 0x00000040, 0x00000080,
    0x00000100, 0x00000200, 0x00000400, 0x00000800,
    0x00001000, 0x00002000, 0x00004000, 0x00008000,
    0x00010000, 0x00020000, 0x00040000, 0x00080000,
    0x00100000, 0x00200000, 0x00400000, 0x00800000,
    0x01000000, 0x02000000, 0x04000000, 0x08000000,
    0x10000000, 0x20000000, 0x40000000, 0x80000000 };

#ifndef MSBYTE_FIRST
static u_long leftmask[32] = {
    0x00000000, 0x00000001, 0x00000003, 0x00000007,
    0x0000000f, 0x0000001f, 0x0000003f, 0x0000007f,
    0x000000ff, 0x000001ff, 0x000003ff, 0x000007ff,
    0x00000fff, 0x00001fff, 0x00003fff, 0x00007fff,
    0x0000ffff, 0x0001ffff, 0x0003ffff, 0x0007ffff,
    0x000fffff, 0x001fffff, 0x003fffff, 0x007fffff,
    0x00ffffff, 0x01ffffff, 0x03ffffff, 0x07ffffff,
    0x0fffffff, 0x1fffffff, 0x3fffffff, 0x7fffffff };
static u_long rightmask[32] = {
    0x00000000, 0x80000000, 0xc0000000, 0xe0000000,
    0xf0000000, 0xf8000000, 0xfc000000, 0xfe000000,
    0xff000000, 0xff800000, 0xffc00000, 0xffe00000,
    0xfff00000, 0xfff80000, 0xfffc0000, 0xfffe0000,
    0xffff0000, 0xffff8000, 0xffffc000, 0xffffe000,
    0xfffff000, 0xfffff800, 0xfffffc00, 0xfffffe00,
    0xffffff00, 0xffffff80, 0xffffffc0, 0xffffffe0,
    0xfffffff0, 0xfffffff8, 0xfffffffc, 0xfffffffe };
#endif /*not MSBYTE_FIRST*/

#endif /*MSBIT_FIRST*/

/* (The odd combinations MSBIT+~MSBYTE and ~MSBIT+MSBYTE could be added.) */

#ifdef MSBYTE_FIRST
static u_long bytemask[4] = { 0xff000000, 0x00ff0000, 0x0000ff00, 0x000000ff };
#else /*MSBYTE_FIRST*/
static u_long bytemask[4] = { 0x000000ff, 0x0000ff00, 0x00ff0000, 0xff000000 };
#endif /*MSBYTE_FIRST*/


/* Forward routines. */

static int raster_blit();


/* Raster operations.  */

/* Performs a bitblit.  Returns 0 on success, -1 on failure. */
int
raster_op( dst, dx, dy, w, h, rop, src, sx, sy )
    struct raster* dst;
    int dx, dy, w, h, rop;
    struct raster* src;
    int sx, sy;
    {
    if ( dst == (struct raster*) 0 )
	return -1;			/* no destination */

    if ( needsrc[RAS_GETOP( rop )] )
	{
	/* Two-operand blit. */
	if ( src == (struct raster*) 0 )
	    return -1;			/* no source */

	/* Clip against source. */
	if ( sx < 0 )
	    {
	    w += sx;
	    sx = 0;
	    }
	if ( sy < 0 )
	    {
	    h += sy;
	    sy = 0;
	    }
	if ( sx + w > src->width )
	    w = src->width - sx;
	if ( sy + h > src->height )
	    h = src->height - sy;

	/* Clip against dest. */
	if ( dx < 0 )
	    {
	    w += dx;
	    sx -= dx;
	    dx = 0;
	    }
	if ( dy < 0 )
	    {
	    h += dy;
	    sy -= dy;
	    dy = 0;
	    }
	if ( dx + w > dst->width )
	    w = dst->width - dx;
	if ( dy + h > dst->height )
	    h = dst->height - dy;

	if ( w <= 0 || h <= 0 )
	    return 0;			/* nothing to do */

	return raster_op_noclip( dst, dx, dy, w, h, rop, src, sx, sy );
	}

    /* No source necessary - one-operand blit. */
    if ( src != (struct raster*) 0 )
	return -1;			/* unwanted source */

    /* Clip against dest. */
    if ( dx < 0 )
	{
	w += dx;
	dx = 0;
	}
    if ( dy < 0 )
	{
	h += dy;
	dy = 0;
	}
    if ( dx + w > dst->width )
	w = dst->width - dx;
    if ( dy + h > dst->height )
	h = dst->height - dy;

    if ( w <= 0 || h <= 0 )
	return 0;			/* nothing to do */

    return raster_op_nosrc_noclip( dst, dx, dy, w, h, rop );
    }

/* Semi-public routine to do a bitblit without clipping.  Returns 0 on
** success, -1 on failure.
*/
int
raster_op_noclip( dst, dx, dy, w, h, rop, src, sx, sy )
    struct raster* dst;
    int dx, dy, w, h, rop;
    struct raster* src;
    int sx, sy;
    {
    int op;

    op = RAS_GETOP( rop );

    if ( src->depth == 1 )
	{
	/* One-bit to ? blit. */
	if ( dst->depth == 1 )
	    {
	    /* One to one blit. */
	    u_long* srclin1;
	    u_long* dstlin1;
	    int srcleftignore, srcrightignore, srclongs;
	    int dstleftignore, dstrightignore, dstlongs;

	    srclin1 = RAS_ADDR( src, sx, sy );
	    dstlin1 = RAS_ADDR( dst, dx, dy );

#ifdef BCOPY_FASTER
	    /* Special-case full-width to full-width copies. */
	    if ( op == RAS_SRC && src->width == w && dst->width == w &&
		 src->linelongs == dst->linelongs && src->linelongs == w >> 5 )
		{
		bcopy(
		    (char*) srclin1, (char*) dstlin1,
		    h * src->linelongs * sizeof(u_long) );
		return 0;
		}
#endif /*BCOPY_FASTER*/

	    srcleftignore = ( sx & 31 );
	    srclongs = ( srcleftignore + w + 31 ) >> 5;
	    srcrightignore = ( srclongs * 32 - w - srcleftignore ) & 31;
	    dstleftignore = ( dx & 31 );
	    dstlongs = ( dstleftignore + w + 31 ) >> 5;
	    dstrightignore = ( dstlongs * 32 - w - dstleftignore ) & 31;

	    return raster_blit(
		src, srclin1, srcleftignore, srcrightignore, srclongs,
		dst, dstlin1, dstleftignore, dstrightignore, dstlongs, h, op );
	    }

	else
	    {
	    /* One to eight, using the color in the rop.  This could
	    ** probably be sped up by handling each four-bit source nybble
	    ** as a group, indexing into a 16-element runtime-constructed
	    ** table of longwords.
	    */
	    u_long* srclin1;
	    u_long* dstlin1;
	    u_long* srclin2;
	    u_long* srclin;
	    u_long* dstlin;
	    register u_long* srclong;
	    register u_long* dstlong;
	    register u_long color, dl;
	    register int srcbit, dstbyte, i;

	    color = RAS_GETCOLOR( rop );
	    if ( color == 0 )
		color = 255;

	    /* Make 32 bits of color so we can do the ROP without shifting. */
	    color |= ( color << 24 ) | ( color << 16 ) | ( color << 8 );

	    /* Don't have to worry about overlapping blits here. */
	    srclin1 = RAS_ADDR( src, sx, sy );
	    srclin2 = srclin1 + h * src->linelongs;
	    dstlin1 = RAS_ADDR( dst, dx, dy );
	    srclin = srclin1;
	    dstlin = dstlin1;
	    while ( srclin != srclin2 )
		{
		srclong = srclin;
		srcbit = sx & 31;
		dstlong = dstlin;
		dstbyte = dx & 3;
		i = w;

		/* WARNING: this code is KNOWN TO FAIL on Sun 3's / CG2's. */
		ROP_SRCDSTCOLOR(
		/*op*/  op,
		/*pre*/ while ( i > 0 )
			    {
			    dl = *dstlong;,
		/*s*/       *srclong & raster_bitmask[srcbit],
		/*d*/       dl,
		/*c*/       color,
		/*pst*/     *dstlong = ( *dstlong & ~bytemask[dstbyte] ) |
				       ( dl & bytemask[dstbyte] );
			    if ( srcbit == 31 )
				{
				srcbit = 0;
				++srclong;
				}
			    else
				++srcbit;
			    if ( dstbyte == 3 )
				{
				dstbyte = 0;
				++dstlong;
				}
			    else
				++dstbyte;
			    --i;
			    } )

		srclin += src->linelongs;
		dstlin += dst->linelongs;
		}
	    }
	}

    else
	{
	/* Eight to eight blit. */
	u_long* srclin1;
	u_long* dstlin1;
	int srcleftignore, srcrightignore, srclongs;
	int dstleftignore, dstrightignore, dstlongs;

	if ( dst->depth != 8 )
	    return -1;		/* depth mismatch */

	srclin1 = RAS_ADDR( src, sx, sy );
	dstlin1 = RAS_ADDR( dst, dx, dy );

#ifdef BCOPY_FASTER
	/* Special-case full-width to full-width copies. */
	if ( op == RAS_SRC && src->width == w && dst->width == w &&
	     src->linelongs == dst->linelongs && src->linelongs == w >> 2 )
	    {
	    bcopy( (char*) srclin1, (char*) dstlin1,
		   h * src->linelongs * sizeof(u_long) );
	    return 0;
	    }
#endif /*BCOPY_FASTER*/

	srcleftignore = ( sx & 3 ) * 8;
	srclongs = ( srcleftignore + w * 8 + 31 ) >> 5;
	srcrightignore = ( srclongs * 32 - w * 8 - srcleftignore ) & 31;
	dstleftignore = ( dx & 3 ) * 8;
	dstlongs = ( dstleftignore + w * 8 + 31 ) >> 5;
	dstrightignore = ( dstlongs * 32 - w * 8 - dstleftignore ) & 31;

	return raster_blit(
	    src, srclin1, srcleftignore, srcrightignore, srclongs,
	    dst, dstlin1, dstleftignore, dstrightignore, dstlongs, h, op );
	}

    return 0;
    }

/* Semi-public routine to do a no-src bitblit without clipping.  Returns 0
** on success, -1 on failure.
*/
int
raster_op_nosrc_noclip( dst, dx, dy, w, h, rop )
    struct raster* dst;
    int dx, dy, w, h, rop;
    {
    int op;

    op = RAS_GETOP( rop );

    if ( dst->depth == 1 )
	{
	/* One-bit no-src blit. */
	u_long* dstlin1;
	u_long* dstlin2;
	u_long* dstlin;
	int dstleftignore, dstrightignore, dstlongs;
	u_long dl, lm, nlm, rm, nrm;
	register u_long* dstlong2;
	register u_long* dstlong;

	dstlin1 = RAS_ADDR( dst, dx, dy );

#ifdef BCOPY_FASTER
	/* Special-case full-width clears. */
	if ( op == RAS_CLEAR && dst->width == w && dst->linelongs == w >> 5 )
	    {
	    bzero( (char*) dstlin1, h * dst->linelongs * sizeof(u_long) );
	    return 0;
	    }
#endif /*BCOPY_FASTER*/

	dstleftignore = ( dx & 31 );
	dstlongs = ( dstleftignore + w + 31 ) >> 5;
	dstrightignore = ( dstlongs * 32 - w - dstleftignore ) & 31;

	dstlin2 = dstlin1 + h * dst->linelongs;
	dstlin = dstlin1;

	if ( dstlongs == 1 )
	    {
	    /* It fits into a single longword. */
	    lm = leftmask[dstleftignore] | rightmask[dstrightignore];
	    nlm = ~lm;
	    while ( dstlin != dstlin2 )
		{
		ROP_DST(
		/*op*/  op,
		/*pre*/ dl = *dstlin;,
		/*d*/   dl,
		/*pst*/ *dstlin = ( *dstlin & lm ) | ( dl & nlm ); )

		dstlin += dst->linelongs;
		}
	    }
	else
	    {
	    lm = leftmask[dstleftignore];
	    rm = rightmask[dstrightignore];
	    nrm = ~rm;
	    nlm = ~lm;

	    while ( dstlin != dstlin2 )
		{
		dstlong = dstlin;
		dstlong2 = dstlong + dstlongs;
		if ( dstrightignore != 0 )
		    --dstlong2;

		/* Leading edge. */
		if ( dstleftignore != 0 )
		    {
		    ROP_DST(
		    /*op*/  op,
		    /*pre*/ dl = *dstlong;,
		    /*d*/   dl,
		    /*pst*/ *dstlong = ( *dstlong & lm ) | ( dl & nlm ); )
		    ++dstlong;
		    }

		/* Main rop. */
		ROP_DST(
		/*op*/  op,
		/*pre*/ while ( dstlong != dstlong2 )
			    {,
		/*d*/       *dstlong,
		/*pst*/     ++dstlong;
			    } )

		/* Trailing edge. */
		if ( dstrightignore != 0 )
		    {
		    ROP_DST(
		    /*op*/  op,
		    /*pre*/ dl = *dstlong;,
		    /*d*/   dl,
		    /*pst*/ *dstlong = ( dl & nrm ) | ( *dstlong & rm ); )
		    }

		dstlin += dst->linelongs;
		}
	    }
	}

    else
	{
	/* Eight-bit no-src blit. */
	register u_long color;
	u_long* dstlin1;
	u_long* dstlin2;
	u_long* dstlin;
	int dstleftignore, dstrightignore, dstlongs;
	u_long dl, lm, nlm, rm, nrm;
	register u_long* dstlong2;
	register u_long* dstlong;

	dstlin1 = RAS_ADDR( dst, dx, dy );

#ifdef BCOPY_FASTER
	/* Special-case full-width clears. */
	if ( op == RAS_CLEAR && dst->width == w && dst->linelongs == w >> 2 )
	    {
	    bzero( (char*) dstlin1, h * dst->linelongs * sizeof(u_long) );
	    return 0;
	    }
#endif /*BCOPY_FASTER*/

	color = RAS_GETCOLOR( rop );
	if ( color == 0 )
	    color = 255;

	/* Make 32 bits of color so we can do the ROP without shifting. */
	color |= ( color << 24 ) | ( color << 16 ) | ( color << 8 );

	dstleftignore = ( dx & 3 ) * 8;
	dstlongs = ( dstleftignore + w * 8 + 31 ) >> 5;
	dstrightignore = ( dstlongs * 32 - w * 8 - dstleftignore ) & 31;

	dstlin2 = dstlin1 + h * dst->linelongs;
	dstlin = dstlin1;

	if ( dstlongs == 1 )
	    {
	    /* It fits into a single longword. */
	    lm = leftmask[dstleftignore] | rightmask[dstrightignore];
	    nlm = ~lm;
	    while ( dstlin != dstlin2 )
		{
		ROP_DSTCOLOR(
		/*op*/  op,
		/*pre*/ dl = *dstlin;,
		/*d*/   dl,
		/*c*/	color,
		/*pst*/ *dstlin = ( *dstlin & lm ) | ( dl & nlm ); )

		dstlin += dst->linelongs;
		}
	    }
	else
	    {
	    lm = leftmask[dstleftignore];
	    rm = rightmask[dstrightignore];
	    nrm = ~rm;
	    nlm = ~lm;
	    while ( dstlin != dstlin2 )
		{
		dstlong = dstlin;
		dstlong2 = dstlong + dstlongs;
		if ( dstrightignore != 0 )
		    --dstlong2;

		/* Leading edge. */
		if ( dstleftignore != 0 )
		    {
		    ROP_DSTCOLOR(
		    /*op*/  op,
		    /*pre*/ dl = *dstlong;,
		    /*d*/   dl,
		    /*c*/   color,
		    /*pst*/ *dstlong = ( *dstlong & lm ) | ( dl & nlm ); )
		    ++dstlong;
		    }

		/* Main rop. */
		ROP_DSTCOLOR(
		/*op*/  op,
		/*pre*/ while ( dstlong != dstlong2 )
			    {,
		/*d*/       *dstlong,
		/*c*/       color,
		/*pst*/     ++dstlong;
			    } )

		/* Trailing edge. */
		if ( dstrightignore != 0 )
		    {
		    ROP_DSTCOLOR(
		    /*op*/  op,
		    /*pre*/ dl = *dstlong;,
		    /*d*/   dl,
		    /*c*/   color,
		    /*pst*/ *dstlong = ( dl & nrm ) | ( *dstlong & rm ); )
		    }

		dstlin += dst->linelongs;
		}
	    }
	}

    return 0;
    }

/* This is a general bitblit routine, handling overlapping source and
** destination.  It's used for both the 1-to-1 and 8-to-8 cases.
*/
static int
raster_blit( src, srclin1, srcleftignore, srcrightignore, srclongs, dst, dstlin1, dstleftignore, dstrightignore, dstlongs, h, op )
    struct raster* src;
    u_long* srclin1;
    int srcleftignore, srcrightignore, srclongs;
    struct raster* dst;
    u_long* dstlin1;
    int dstleftignore, dstrightignore, dstlongs;
    int h, op;
    {
    u_long* srclin2;
    u_long* dstlin2;
    int srclininc, dstlininc;
    u_long* srclin;
    u_long* dstlin;
    register int prevleftshift, currrightshift;
    int longinc;
    register u_long* srclong;
    register u_long* dstlong;
    register u_long* dstlong2;
    register u_long dl, lm, nlm, rm, nrm;

    prevleftshift = ( srcleftignore - dstleftignore ) & 31;

    srclin2 = srclin1 + h * src->linelongs;
    dstlin2 = dstlin1 + h * dst->linelongs;
    srclininc = src->linelongs;
    dstlininc = dst->linelongs;
    longinc = 1;

    /* Check for overlaps. */
    if ( ( dstlin1 >= srclin1 && dstlin1 < srclin1 + srclongs ) ||
	 ( srclin1 >= dstlin1 && srclin1 < dstlin1 + dstlongs ) )
	{
	/* Horizontal overlap.  Should we reverse? */
	if ( srclin1 < dstlin1 )
	    {
	    longinc = -1;
	    srclin1 += srclongs - 1;
	    srclin2 += srclongs - 1;
	    dstlin1 += dstlongs - 1;
	    }
	}
    else if ( ( dstlin1 >= srclin1 && dstlin1 < srclin2 ) ||
	      ( srclin1 >= dstlin1 && srclin1 < dstlin2 ) )
	{
	/* Vertical overlap.  Should we reverse? */
	if ( srclin1 < dstlin1 )
	    {
	    srclin2 = srclin1 - srclininc;
	    srclin1 += ( h - 1 ) * srclininc;
	    dstlin1 += ( h - 1 ) * dstlininc;
	    srclininc = -srclininc;
	    dstlininc = -dstlininc;
	    }
	}
    srclin = srclin1;
    dstlin = dstlin1;

    if ( prevleftshift == 0 )
	{
	/* The bits line up, no shifting necessary. */
	if ( dstlongs == 1 )
	    {
	    /* It all fits into a single longword. */
	    lm = leftmask[dstleftignore] | rightmask[dstrightignore];
	    nlm = ~lm;
	    while ( srclin != srclin2 )
		{
		ROP_SRCDST(
		/*op*/  op,
		/*pre*/ dl = *dstlin;,
		/*s*/   *srclin,
		/*d*/   dl,
		/*pst*/ *dstlin = ( *dstlin & lm ) | ( dl & nlm ); )

		srclin += srclininc;
		dstlin += dstlininc;
		}
	    }
	else
	    {
	    /* Multiple longwords. */
	    lm = leftmask[dstleftignore];
	    rm = rightmask[dstrightignore];
	    nrm = ~rm;
	    nlm = ~lm;
	    if ( longinc == 1 )
		{
		/* Left to right. */
		while ( srclin != srclin2 )
		    {
		    srclong = srclin;
		    dstlong = dstlin;
		    dstlong2 = dstlong + dstlongs;
		    if ( dstrightignore != 0 )
			--dstlong2;

		    /* Leading edge. */
		    if ( dstleftignore != 0 )
			{
			ROP_SRCDST(
			/*op*/  op,
			/*pre*/ dl = *dstlong;,
			/*s*/   *srclong,
			/*d*/   dl,
			/*pst*/ *dstlong = ( *dstlong & lm ) | ( dl & nlm ); )
			++srclong;
			++dstlong;
			}

		    /* Main rop. */
		    ROP_SRCDST(
		    /*op*/  op,
		    /*pre*/ while ( dstlong != dstlong2 )
				{,
		    /*s*/       *srclong,
		    /*d*/       *dstlong,
		    /*pst*/     ++srclong;
				++dstlong;
				} )

		    /* Trailing edge. */
		    if ( dstrightignore != 0 )
			{
			ROP_SRCDST(
			/*op*/  op,
			/*pre*/ dl = *dstlong;,
			/*s*/   *srclong,
			/*d*/   dl,
			/*pst*/ *dstlong = ( dl & nrm ) | ( *dstlong & rm ); )
			}

		    srclin += srclininc;
		    dstlin += dstlininc;
		    }
		}
	    else
		{
		/* Right to left. */
		while ( srclin != srclin2 )
		    {
		    srclong = srclin;
		    dstlong = dstlin;
		    dstlong2 = dstlong - dstlongs;
		    if ( dstleftignore != 0 )
			++dstlong2;

		    /* Leading edge. */
		    if ( dstrightignore != 0 )
			{
			ROP_SRCDST(
			/*op*/  op,
			/*pre*/ dl = *dstlong;,
			/*s*/   *srclong,
			/*d*/   dl,
			/*pst*/ *dstlong = ( dl & nrm ) | ( *dstlong & rm ); )
			--srclong;
			--dstlong;
			}

		    /* Main rop. */
		    ROP_SRCDST(
		    /*op*/  op,
		    /*pre*/ while ( dstlong != dstlong2 )
				{,
		    /*s*/       *srclong,
		    /*d*/       *dstlong,
		    /*pst*/     --srclong;
				--dstlong;
				} )

		    /* Trailing edge. */
		    if ( dstleftignore != 0 )
			{
			ROP_SRCDST(
			/*op*/  op,
			/*pre*/ dl = *dstlong;,
			/*s*/   *srclong,
			/*d*/   dl,
			/*pst*/ *dstlong = ( *dstlong & lm ) | ( dl & nlm ); )
			}

		    srclin += srclininc;
		    dstlin += dstlininc;
		    }
		}
	    }
	}

    else
	{
	/* General case, with shifting and everything. */
	register u_long sl, prevsl;

	currrightshift = 32 - prevleftshift;
	if ( srclongs == 1 && dstlongs == 1 )
	    {
	    /* It fits into a single longword, with a shift. */
	    lm = leftmask[dstleftignore] | rightmask[dstrightignore];
	    nlm = ~lm;
	    if ( srcleftignore > dstleftignore )
		{
		while ( srclin != srclin2 )
		    {
		    ROP_SRCDST(
		    /*op*/  op,
		    /*pre*/ dl = *dstlin;,
		    /*s*/   *srclin << prevleftshift,
		    /*d*/   dl,
		    /*pst*/ *dstlin = ( *dstlin & lm ) | ( dl & nlm ); )

		    srclin += srclininc;
		    dstlin += dstlininc;
		    }
		}
	    else
		{
		while ( srclin != srclin2 )
		    {
		    ROP_SRCDST(
		    /*op*/  op,
		    /*pre*/ dl = *dstlin;,
		    /*s*/   *srclin >> currrightshift,
		    /*d*/   dl,
		    /*pst*/ *dstlin = ( *dstlin & lm ) | ( dl & nlm ); )

		    srclin += srclininc;
		    dstlin += dstlininc;
		    }
		}
	    }
	else
	    {
	    /* Multiple longwords. */
	    lm = leftmask[dstleftignore];
	    rm = rightmask[dstrightignore];
	    nrm = ~rm;
	    nlm = ~lm;
	    if ( longinc == 1 )
		{
		/* Left to right. */
		while ( srclin != srclin2 )
		    {
		    srclong = srclin;
		    dstlong = dstlin;
		    dstlong2 = dstlong + dstlongs;
		    if ( srcleftignore > dstleftignore )
			prevsl = *srclong++ << prevleftshift;
		    else
			prevsl = 0;
		    if ( dstrightignore != 0 )
			--dstlong2;

		    /* Leading edge. */
		    if ( dstleftignore != 0 )
			{
			ROP_SRCDST(
			/*op*/  op,
			/*pre*/ sl = *srclong;
				dl = *dstlong;,
			/*s*/   prevsl | ( sl >> currrightshift ),
			/*d*/   dl,
			/*pst*/ *dstlong = ( *dstlong & lm ) | ( dl & nlm ); )
			prevsl = sl << prevleftshift;
			++srclong;
			++dstlong;
			}

		    /* Main rop. */
		    ROP_SRCDST(
		    /*op*/  op,
		    /*pre*/ while ( dstlong != dstlong2 )
				{
				sl = *srclong;,
		    /*s*/       prevsl | ( sl >> currrightshift ),
		    /*d*/       *dstlong,
		    /*pst*/     prevsl = sl << prevleftshift;
				++srclong;
				++dstlong;
				} )

		    /* Trailing edge. */
		    if ( dstrightignore != 0 )
			{
			ROP_SRCDST(
			/*op*/  op,
			/*pre*/ dl = *dstlong;,
			/*s*/   prevsl | ( *srclong >> currrightshift ),
			/*d*/   dl,
			/*pst*/ *dstlong = ( dl & nrm ) | ( *dstlong & rm ); )
			}

		    srclin += srclininc;
		    dstlin += dstlininc;
		    }
		}
	    else
		{
		/* Right to left. */
		while ( srclin != srclin2 )
		    {
		    srclong = srclin;
		    dstlong = dstlin;
		    dstlong2 = dstlong - dstlongs;
		    if ( srcrightignore > dstrightignore )
			prevsl = *srclong-- >> currrightshift;
		    else
			prevsl = 0;
		    if ( dstleftignore != 0 )
			++dstlong2;

		    /* Leading edge. */
		    if ( dstrightignore != 0 )
			{
			ROP_SRCDST(
			/*op*/  op,
			/*pre*/ sl = *srclong;
				dl = *dstlong;,
			/*s*/   prevsl | ( sl << prevleftshift ),
			/*d*/   dl,
			/*pst*/ *dstlong = ( dl & nrm ) | ( *dstlong & rm ); )
			prevsl = sl >> currrightshift;
			--srclong;
			--dstlong;
			}

		    /* Main rop. */
		    ROP_SRCDST(
		    /*op*/  op,
		    /*pre*/ while ( dstlong != dstlong2 )
				{
				sl = *srclong;,
		    /*s*/       prevsl | ( sl << prevleftshift ),
		    /*d*/       *dstlong,
		    /*pst*/     prevsl = sl >> currrightshift;
				--srclong;
				--dstlong;
				} )

		    /* Trailing edge. */
		    if ( dstleftignore != 0 )
			{
			ROP_SRCDST(
			/*op*/  op,
			/*pre*/ dl = *dstlong;,
			/*s*/   prevsl | ( *srclong << prevleftshift ),
			/*d*/   dl,
			/*pst*/ *dstlong = ( *dstlong & lm ) | ( dl & nlm ); )
			}

		    srclin += srclininc;
		    dstlin += dstlininc;
		    }
		}
	    }
	}

    return 0;
    }
