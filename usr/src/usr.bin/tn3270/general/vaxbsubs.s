/*
 *	@(#)vaxbsubs.s	3.1  10/29/86
 */

/*
 *	Copyright (c) 1984, 1985, 1986 by the Regents of the
 *	University of California and by Gregory Glenn Minshall.
 *
 *	Permission to use, copy, modify, and distribute these
 *	programs and their documentation for any purpose and
 *	without fee is hereby granted, provided that this
 *	copyright and permission appear on all copies and
 *	supporting documentation, the name of the Regents of
 *	the University of California not be used in advertising
 *	or publicity pertaining to distribution of the programs
 *	without specific prior permission, and notice be given in
 *	supporting documentation that copying and distribution is
 *	by permission of the Regents of the University of California
 *	and by Gregory Glenn Minshall.  Neither the Regents of the
 *	University of California nor Gregory Glenn Minshall make
 *	representations about the suitability of this software
 *	for any purpose.  It is provided "as is" without
 *	express or implied warranty.
 */

/* This is taken from bcmp.s from 4.2.
 * The output of bunequal is the offset of the byte which didn't match;
 * if all the bytes match, then we return n.
 *
 * BUGNOTE:  This has no chance of working for lengths greater than 64K.
 *		(so, if you use this somewhere else, you may need to
 *		fix it...)
 */

/* bunequal(s1, s2, n) */

#include "defs.h"

ENTRY(bunequal)
	movl	4(ap),r1
	movl	8(ap),r3
	movl	12(ap),r4
1:
	movzwl	$65535,r0
	cmpl	r4,r0
	jleq	2f
	subl2	r0,r4
	cmpc3	r0,(r1),(r3)
	jeql	1b
	addl2	r4,r0
	/* changes... */
	subl3	r0,12(ap),r0
	/* end of changes for bunequal... */
	ret
2:
	cmpc3	r4,(r1),(r3)
	/* changes... */
	subl3	r0,12(ap),r0
	/* end of changes for bunequal... */
	ret




/* brand new code, using the above as base... */
/* bskip(s1, n, b) : finds the first occurrence of any byte != 'b' in the 'n'
 * bytes beginning at 's1'.
 *
 * BUGNOTE:  This has no chance of working for lengths greater than 64K.
 *		(so, if you use this somewhere else, you may need to
 *		fix it...)
 */

ENTRY(bskip)
	movl	4(ap),r1
	movl	8(ap),r3
	movl	12(ap),r4
1:
	movzwl	$65535,r0
	cmpl	r3,r0
	jleq	2f
	subl2	r0,r3
	skpc	r4,r0,(r1)
	jeql	1b
	addl2	r3,r0
	subl3	r0,8(ap),r0
	ret
2:
	skpc	r4,r3,(r1)
	subl3	r0,8(ap),r0
	ret
