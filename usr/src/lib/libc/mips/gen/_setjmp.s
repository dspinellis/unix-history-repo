/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 */

#include <machine/reg.h>
#include <machine/machAsmDefs.h>

#if defined(LIBC_SCCS) && !defined(lint)
	ASMSTR("@(#)_setjmp.s	8.1 (Berkeley) %G%")
#endif /* LIBC_SCCS and not lint */

/*
 * C library -- _setjmp, _longjmp
 *
 *	_longjmp(a,v)
 * will generate a "return(v)" from
 * the last call to
 *	_setjmp(a)
 * by restoring registers from the stack,
 * The previous signal state is NOT restored.
 */

	.set	noreorder

LEAF(_setjmp)
	li	v0, 0xACEDBADE			# sigcontext magic number
	sw	ra, (2 * 4)(a0)			# sc_pc = return address
	sw	v0, (3 * 4)(a0)			#   saved in sc_regs[0]
	sw	s0, ((S0 + 3) * 4)(a0)
	sw	s1, ((S1 + 3) * 4)(a0)
	sw	s2, ((S2 + 3) * 4)(a0)
	sw	s3, ((S3 + 3) * 4)(a0)
	sw	s4, ((S4 + 3) * 4)(a0)
	sw	s5, ((S5 + 3) * 4)(a0)
	sw	s6, ((S6 + 3) * 4)(a0)
	sw	s7, ((S7 + 3) * 4)(a0)
	sw	sp, ((SP + 3) * 4)(a0)
	sw	s8, ((S8 + 3) * 4)(a0)
	cfc1	v0, $31				# too bad cant check if FP used
	swc1	$f20, ((20 + 38) * 4)(a0)
	swc1	$f21, ((21 + 38) * 4)(a0)
	swc1	$f22, ((22 + 38) * 4)(a0)
	swc1	$f23, ((23 + 38) * 4)(a0)
	swc1	$f24, ((24 + 38) * 4)(a0)
	swc1	$f25, ((25 + 38) * 4)(a0)
	swc1	$f26, ((26 + 38) * 4)(a0)
	swc1	$f27, ((27 + 38) * 4)(a0)
	swc1	$f28, ((28 + 38) * 4)(a0)
	swc1	$f29, ((29 + 38) * 4)(a0)
	swc1	$f30, ((30 + 38) * 4)(a0)
	swc1	$f31, ((31 + 38) * 4)(a0)
	sw	v0, ((32 + 38) * 4)(a0)
	j	ra
	move	v0, zero
END(_setjmp)

LEAF(_longjmp)
	lw	v0, (3 * 4)(a0)			# get magic number
	lw	ra, (2 * 4)(a0)
	bne	v0, 0xACEDBADE, botch		# jump if error
	lw	s0, ((S0 + 3) * 4)(a0)
	lw	s1, ((S1 + 3) * 4)(a0)
	lw	s2, ((S2 + 3) * 4)(a0)
	lw	s3, ((S3 + 3) * 4)(a0)
	lw	s4, ((S4 + 3) * 4)(a0)
	lw	s5, ((S5 + 3) * 4)(a0)
	lw	s6, ((S6 + 3) * 4)(a0)
	lw	s7, ((S7 + 3) * 4)(a0)
	lw	v0, ((32 + 38) * 4)(a0)		# get fpu status
	lw	sp, ((SP + 3) * 4)(a0)
	lw	s8, ((S8 + 3) * 4)(a0)
	ctc1	v0, $31
	lwc1	$f20, ((20 + 38) * 4)(a0)
	lwc1	$f21, ((21 + 38) * 4)(a0)
	lwc1	$f22, ((22 + 38) * 4)(a0)
	lwc1	$f23, ((23 + 38) * 4)(a0)
	lwc1	$f24, ((24 + 38) * 4)(a0)
	lwc1	$f25, ((25 + 38) * 4)(a0)
	lwc1	$f26, ((26 + 38) * 4)(a0)
	lwc1	$f27, ((27 + 38) * 4)(a0)
	lwc1	$f28, ((28 + 38) * 4)(a0)
	lwc1	$f29, ((29 + 38) * 4)(a0)
	lwc1	$f30, ((30 + 38) * 4)(a0)
	lwc1	$f31, ((31 + 38) * 4)(a0)
	j	ra
	move	v0, a1
botch:
	jal	longjmperror
	nop
	jal	abort
	nop
END(_longjmp)
