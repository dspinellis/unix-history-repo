/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
	.asciz "@(#)s_cmp_s.s	5.2 (Berkeley) %G%"
#endif /* not lint */

.data
.text
LL0:.align	1
.globl	_s_cmp
.set	MASK__,0x1004
.data
.text
_s_cmp:.word	MASK__
movl	4(fp),r0		/* a */
movl	8(fp),r1		/* b */
movl	12(fp),r12		/* la */
cmpl	r12,16(fp)		/* if (la <= lb) */
jgtr	L17
movl	r12, r2			/* compare according to la */
cmps3
jeql	L2			/* if not equal */

L20:
cvtbl	(r0), r0		/* return(*a - *b) */
cvtbl	(r1), r1
subl2	r1,r0
ret

L2:
cmpl	r12,16(fp)		/* if (la == lb) */
jneq	L50
clrl	r0			/* then strings are equal */
ret

L50:
addl3	8(fp), 16(fp), r12	/* r12 = bend */

L3:
cmpb	(r1), $32		/* if *b != space */
jeql	L4
cvtbl	(r1), r1
movl	$32, r0			/* return(' ' - *b) */
subl2	r1, r0
ret

L4:				/* else loop */
incl	r1
cmpl	r1, r12
jlssu	L3			/* till bend */
clrl	r0
ret				/* strings equal: return(0) */


L17:				/* else */
movl	16(fp), r2		/* compare according to lb */
cmps3
jneq	L20			/* if not equal */
				/* return(*a - *b) */
addl3	4(fp), 12(fp), r12	/* r12 = aend */

L30:
cmpb	(r0), $32		/* if *a != space */
jeql	L40
cvtbl	(r0), r0
movl	$32, r1			/* return(*a - ' ') */
subl2	r1, r0
ret

L40:				/* else loop */
incl	r0
cmpl	r0, r12
jlssu	L30			/* till bend */
clrl	r0
ret				/* strings equal: return(0) */

