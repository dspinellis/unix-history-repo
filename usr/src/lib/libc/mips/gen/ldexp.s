/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 */

#include <machine/machAsmDefs.h>

#if defined(LIBC_SCCS) && !defined(lint)
	ASMSTR("@(#)ldexp.s	8.1 (Berkeley) %G%")
#endif /* LIBC_SCCS and not lint */

#define DEXP_INF	0x7ff
#define DEXP_BIAS	1023
#define DEXP_MIN	-1022
#define DEXP_MAX	1023
#define DFRAC_BITS	52
#define DIMPL_ONE	0x00100000
#define DLEAD_ZEROS	31 - 20
#define STICKYBIT	1
#define GUARDBIT	0x80000000
#define DSIGNAL_NAN	0x00040000
#define DQUIET_NAN0	0x0007ffff
#define DQUIET_NAN1	0xffffffff

/*
 * double ldexp(x, N)
 *	double x; int N;
 *
 * Return x * (2**N), for integer values N.
 */
LEAF(ldexp)
	mfc1	v1, $f13		# get MSW of x
	mfc1	t3, $f12		# get LSW of x
	sll	t1, v1, 1		# get x exponent
	srl	t1, t1, 32 - 11
	beq	t1, DEXP_INF, 9f	# is it a NAN or infinity?
	beq	t1, zero, 1f		# zero or denormalized number?
	addu	t1, t1, a2		# scale exponent
	sll	v0, a2, 20		# position N for addition
	bge	t1, DEXP_INF, 8f	# overflow?
	addu	v0, v0, v1		# multiply by (2**N)
	ble	t1, zero, 4f		# underflow?
	mtc1	v0, $f1			# save MSW of result
	mtc1	t3, $f0			# save LSW of result
	j	ra
1:
	sll	t2, v1, 32 - 20		# get x fraction
	srl	t2, t2, 32 - 20
	srl	t0, v1, 31		# get x sign
	bne	t2, zero, 1f
	beq	t3, zero, 9f		# result is zero
1:
/*
 * Find out how many leading zero bits are in t2,t3 and put in t9.
 */
	move	v0, t2
	move	t9, zero
	bne	t2, zero, 1f
	move	v0, t3
	addu	t9, 32
1:
	srl	t4, v0, 16
	bne	t4, zero, 1f
	addu	t9, 16
	sll	v0, 16
1:
	srl	t4, v0, 24
	bne	t4, zero, 1f
	addu	t9, 8
	sll	v0, 8
1:
	srl	t4, v0, 28
	bne	t4, zero, 1f
	addu	t9, 4
	sll	v0, 4
1:
	srl	t4, v0, 30
	bne	t4, zero, 1f
	addu	t9, 2
	sll	v0, 2
1:
	srl	t4, v0, 31
	bne	t4, zero, 1f
	addu	t9, 1
/*
 * Now shift t2,t3 the correct number of bits.
 */
1:
	subu	t9, t9, DLEAD_ZEROS	# dont count normal leading zeros
	li	t1, DEXP_MIN + DEXP_BIAS
	subu	t1, t1, t9		# adjust exponent
	addu	t1, t1, a2		# scale exponent
	li	v0, 32
	blt	t9, v0, 1f
	subu	t9, t9, v0		# shift fraction left >= 32 bits
	sll	t2, t3, t9
	move	t3, zero
	b	2f
1:
	subu	v0, v0, t9		# shift fraction left < 32 bits
	sll	t2, t2, t9
	srl	t4, t3, v0
	or	t2, t2, t4
	sll	t3, t3, t9
2:
	bge	t1, DEXP_INF, 8f	# overflow?
	ble	t1, zero, 4f		# underflow?
	sll	t2, t2, 32 - 20		# clear implied one bit
	srl	t2, t2, 32 - 20
3:
	sll	t1, t1, 31 - 11		# reposition exponent
	sll	t0, t0, 31		# reposition sign
	or	t0, t0, t1		# put result back together
	or	t0, t0, t2
	mtc1	t0, $f1			# save MSW of result
	mtc1	t3, $f0			# save LSW of result
	j	ra
4:
	li	v0, 0x80000000
	ble	t1, -52, 7f		# is result too small for denorm?
	sll	t2, v1, 31 - 20		# clear exponent, extract fraction
	or	t2, t2, v0		# set implied one bit
	blt	t1, -30, 2f		# will all bits in t3 be shifted out?
	srl	t2, t2, 31 - 20		# shift fraction back to normal position
	subu	t1, t1, 1
	sll	t4, t2, t1		# shift right t2,t3 based on exponent
	srl	t8, t3, t1		# save bits shifted out
	negu	t1
	srl	t3, t3, t1
	or	t3, t3, t4
	srl	t2, t2, t1
	bge	t8, zero, 1f		# does result need to be rounded?
	addu	t3, t3, 1		# round result
	sltu	t4, t3, 1
	sll	t8, t8, 1
	addu	t2, t2, t4
	bne	t8, zero, 1f		# round result to nearest
	and	t3, t3, ~1
1:
	mtc1	t3, $f0			# save denormalized result (LSW)
	mtc1	t2, $f1			# save denormalized result (MSW)
	bge	v1, zero, 1f		# should result be negative?
	neg.d	$f0, $f0		# negate result
1:
	j	ra
2:
	mtc1	zero, $f1		# exponent and upper fraction
	addu	t1, t1, 20		# compute amount to shift right by
	sll	t8, t2, t1		# save bits shifted out
	negu	t1
	srl	t3, t2, t1
	bge	t8, zero, 1f		# does result need to be rounded?
	addu	t3, t3, 1		# round result
	sltu	t4, t3, 1
	sll	t8, t8, 1
	mtc1	t4, $f1			# exponent and upper fraction
	bne	t8, zero, 1f		# round result to nearest
	and	t3, t3, ~1
1:
	mtc1	t3, $f0
	bge	v1, zero, 1f		# is result negative?
	neg.d	$f0, $f0		# negate result
1:
	j	ra
7:
	mtc1	zero, $f0		# result is zero
	mtc1	zero, $f1
	beq	t0, zero, 1f		# is result positive?
	neg.d	$f0, $f0		# negate result
1:
	j	ra
8:
	li	t1, 0x7ff00000		# result is infinity (MSW)
	mtc1	t1, $f1	
	mtc1	zero, $f0		# result is infinity (LSW)
	bge	v1, zero, 1f		# should result be negative infinity?
	neg.d	$f0, $f0		# result is negative infinity
1:
	add.d	$f0, $f0		# cause overflow faults if enabled
	j	ra
9:
	mov.d	$f0, $f12		# yes, result is just x
	j	ra
END(ldexp)
