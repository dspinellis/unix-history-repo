/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * %sccs.include.redist.c%
 *
 * from: $Header: ffs.s,v 1.3 92/07/07 00:23:57 torek Exp $
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)ffs.s	8.1 (Berkeley) %G%"
#endif  /* LIBC_SCCS and not lint */

#include "DEFS.h"

/*
 * ffs returns the number of the rightmost bit set in its argument,
 * i.e., the lowest value such that (x & (ffs(x) - 1)) is nonzero.
 * If no bits are set, ffs returns 0.
 *
 * We use a table lookup on each byte.
 *
 * In each section below, %o1 is the current byte (0, 1, 2, or 3).
 * The last byte is handled specially: for the first three,
 * if that byte is nonzero, we return the table value
 * (plus 0, 8, or 16 for the byte number), but for the last
 * one, we just return the table value plus 24.  This means
 * that ffstab[0] must be -24 so that ffs(0) will return 0.
 */
ENTRY(ffs)
	set	ffstab, %o2
	andcc	%o0, 0xff, %o1	! get low byte
	be,a	1f		! try again if 0
	 srl	%o0, 8, %o0	! delay slot, get ready for next byte

	retl			! return ffstab[%o1]
	 ldsb	[%o2 + %o1], %o0

1:
	andcc	%o0, 0xff, %o1	! byte 1 like byte 0...
	be,a	2f
	 srl	%o0, 8, %o0	! (use delay to prepare for byte 2)

	ldsb	[%o2 + %o1], %o0
	retl			! return ffstab[%o1] + 8
	 add	%o0, 8, %o0

2:
	andcc	%o0, 0xff, %o1
	be,a	3f
	 srl	%o0, 8, %o0	! (prepare for byte 3)

	ldsb	[%o2 + %o1], %o0
	retl			! return ffstab[%o1] + 16
	 add	%o0, 16, %o0

3:				! just return ffstab[%o0] + 24
	ldsb	[%o2 + %o0], %o0
	retl
	 add	%o0, 24, %o0

ffstab:
	.byte	-24,1,2,1,3,1,2,1,4,1,2,1,3,1,2,1 /* 00-0f */
	.byte	5,1,2,1,3,1,2,1,4,1,2,1,3,1,2,1	/* 10-1f */
	.byte	6,1,2,1,3,1,2,1,4,1,2,1,3,1,2,1	/* 20-2f */
	.byte	5,1,2,1,3,1,2,1,4,1,2,1,3,1,2,1	/* 30-3f */
	.byte	7,1,2,1,3,1,2,1,4,1,2,1,3,1,2,1	/* 40-4f */
	.byte	5,1,2,1,3,1,2,1,4,1,2,1,3,1,2,1	/* 50-5f */
	.byte	6,1,2,1,3,1,2,1,4,1,2,1,3,1,2,1	/* 60-6f */
	.byte	5,1,2,1,3,1,2,1,4,1,2,1,3,1,2,1	/* 70-7f */
	.byte	8,1,2,1,3,1,2,1,4,1,2,1,3,1,2,1	/* 80-8f */
	.byte	5,1,2,1,3,1,2,1,4,1,2,1,3,1,2,1	/* 10-9f */
	.byte	6,1,2,1,3,1,2,1,4,1,2,1,3,1,2,1	/* a0-af */
	.byte	5,1,2,1,3,1,2,1,4,1,2,1,3,1,2,1	/* b0-bf */
	.byte	7,1,2,1,3,1,2,1,4,1,2,1,3,1,2,1	/* c0-cf */
	.byte	5,1,2,1,3,1,2,1,4,1,2,1,3,1,2,1	/* d0-df */
	.byte	6,1,2,1,3,1,2,1,4,1,2,1,3,1,2,1	/* e0-ef */
	.byte	5,1,2,1,3,1,2,1,4,1,2,1,3,1,2,1	/* f0-ff */
