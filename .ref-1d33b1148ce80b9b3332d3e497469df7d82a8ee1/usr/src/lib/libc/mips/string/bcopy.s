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
	ASMSTR("@(#)bcopy.s	8.1 (Berkeley) %G%")
#endif /* LIBC_SCCS and not lint */

/* bcopy(s1, s2, n) */

#ifdef MIPSEL
#	define	LWHI	lwr
#	define	LWLO	lwl
#	define	SWHI	swr
#	define	SWLO	swl
#endif
#ifdef MIPSEB
#	define	LWHI	lwl
#	define	LWLO	lwr
#	define	SWHI	swl
#	define	SWLO	swr
#endif

LEAF(bcopy)
	.set	noreorder
	addu	t0, a0, a2		# t0 = end of s1 region
	sltu	t1, a1, t0
	sltu	t2, a0, a1
	and	t1, t1, t2		# t1 = true if from < to < (from+len)
	beq	t1, zero, forward	# non overlapping, do forward copy
	slt	t2, a2, 12		# check for small copy

	ble	a2, zero, 2f
	addu	t1, a1, a2		# t1 = end of to region
1:
	lb	v0, -1(t0)		# copy bytes backwards,
	subu	t0, t0, 1		#   doesnt happen often so do slow way
	subu	t1, t1, 1
	bne	t0, a0, 1b
	sb	v0, 0(t1)
2:
	j	ra
	nop
forward:
	bne	t2, zero, smallcpy	# do a small bcopy
	xor	v0, a0, a1		# compare low two bits of addresses
	and	v0, v0, 3
	subu	a3, zero, a1		# compute # bytes to word align address
	beq	v0, zero, aligned	# addresses can be word aligned
	and	a3, a3, 3

	beq	a3, zero, 1f
	subu	a2, a2, a3		# subtract from remaining count
	LWHI	v0, 0(a0)		# get next 4 bytes (unaligned)
	LWLO	v0, 3(a0)
	addu	a0, a0, a3
	SWHI	v0, 0(a1)		# store 1, 2, or 3 bytes to align a1
	addu	a1, a1, a3
1:
	and	v0, a2, 3		# compute number of words left
	subu	a3, a2, v0
	move	a2, v0
	addu	a3, a3, a0		# compute ending address
2:
	LWHI	v0, 0(a0)		# copy words a0 unaligned, a1 aligned
	LWLO	v0, 3(a0)
	addu	a0, a0, 4
	addu	a1, a1, 4
	bne	a0, a3, 2b
	sw	v0, -4(a1)
	b	smallcpy
	nop
aligned:
	beq	a3, zero, 1f
	subu	a2, a2, a3		# subtract from remaining count
	LWHI	v0, 0(a0)		# copy 1, 2, or 3 bytes to align
	addu	a0, a0, a3
	SWHI	v0, 0(a1)
	addu	a1, a1, a3
1:
	and	v0, a2, 3		# compute number of whole words left
	subu	a3, a2, v0
	move	a2, v0
	addu	a3, a3, a0		# compute ending address
2:
	lw	v0, 0(a0)		# copy words
	addu	a0, a0, 4
	addu	a1, a1, 4
	bne	a0, a3, 2b
	sw	v0, -4(a1)
smallcpy:
	ble	a2, zero, 2f
	addu	a3, a2, a0		# compute ending address
1:
	lbu	v0, 0(a0)		# copy bytes
	addu	a0, a0, 1
	addu	a1, a1, 1
	bne	a0, a3, 1b
	sb	v0, -1(a1)
2:
	j	ra
	nop
	.set	reorder
END(bcopy)
