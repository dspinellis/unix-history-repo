/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 */

#include "DEFS.h"

#if defined(LIBC_SCCS) && !defined(lint)
	ASMSTR("@(#)ldexp.s	5.2 (Berkeley) %G%")
#endif /* LIBC_SCCS and not lint */

	.sdata
infinity:
	.word	0x7ff00000	# IEEE infinity
	.text

/*
 * double ldexp(val, exp)
 * returns: val * (2**exp), for integer exp
 */
LEAF(ldexp)
	.set	noreorder
	mfc1	t0, $f13
	mov.d	$f0, $f12
	sll	t1, t0, 1
	srl	t1, t1, 21
	addu	t1, t1, a2
	blez	t1, 3f
	slti	t2, t1, 2047
	beq	t2, zero, 2f
	sll	a2, a2, 20
	addu	t0, t0, a2
	mtc1	t0, $f1
1:
	j	ra
	nop
2:
	lwc1	$f1, infinity
	bgez	t0, 1b
	mtc1	zero, $f0
	j	ra
	neg.d	$f0, $f0
3:
	blt	t1, -51, 9f
	mfc1	t5, $f13
	li	t2, 0x80000000
	sll	t5, t5, 11
	blt	t1, -30, 7f
	or	t5, t5, t2
	srl	t5, t5, 11
	mfc1	t4, $f12
	addiu	t1, t1, -1
	sll	t3, t5, t1
	srl	t2, t4, t1
	subu	t1, zero, t1
	srl	t4, t4, t1
	or	t4, t4, t3
	bgez	t2, 6f
	srl	t5, t5, t1
	addiu	t4, t4, 1
	sltiu	t6, t4, 1
	sll	t2, t2, 1
	bne	t2, zero, 6f
	addu	t5, t5, t6
	and	t4, t4, ~1
6:
	mtc1	t4, $f0
	bgez	t0, 1b
	mtc1	t5, $f1
	j	ra
	neg.d	$f0, $f0
7:
	mtc1	zero, $f1
	addiu	t1, t1, 20
	sll	t2, t5, t1
	subu	t1, zero, t1
	bgez	t2, 8f
	srl	t4, t5, t1
	addiu	t4, t4, 1
	sltiu	t6, t4, 1
	sll	t2, t2, 1
	bne	t2, zero, 8f
	mtc1	t6, $f1
	and	t4, t4, ~1
8:
	bgez	t0, 1b
	mtc1	t4, $f0
	j	ra
	neg.d	$f0, $f0
9:
	mtc1	zero, $f0
	bgez	t0, 1b
	mtc1	zero, $f1
	j	ra
	neg.d	$f0, $f0
	.set	reorder
END(ldexp)
