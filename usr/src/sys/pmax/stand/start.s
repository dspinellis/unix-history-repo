/*
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)start.s	7.3 (Berkeley) %G%
 *
 * start.s -
 *
 *     Contains code that is the first executed at boot time.
 *
 *	Copyright (C) 1989 Digital Equipment Corporation.
 *	Permission to use, copy, modify, and distribute this software and
 *	its documentation for any purpose and without fee is hereby granted,
 *	provided that the above copyright notice appears in all copies.  
 *	Digital Equipment Corporation makes no representations about the
 *	suitability of this software for any purpose.  It is provided "as is"
 *	without express or implied warranty.
 *
 * from: $Header: /sprite/src/boot/decprom/ds3100.md/RCS/start.s,
 *	v 1.1 90/02/16 16:19:39 shirriff Exp $ SPRITE (DECWRL)
 */

#include "../include/regdef.h"
#include "../include/machConst.h"

/*
 * Amount to take off of the stack for the benefit of the debugger.
 */
#define START_FRAME	((4 * 4) + 4 + 4)
#define Init	0xbfc00018

	.globl	start
start:
	.set	noreorder
	mtc0	zero, MACH_COP_0_STATUS_REG	# Disable interrupts
	li	sp, MACH_CODE_START - START_FRAME
	la	gp, _gp
	sw	zero, START_FRAME - 4(sp)	# Zero out old ra for debugger
	sw	zero, START_FRAME - 8(sp)	# Zero out old fp for debugger
	jal	main				# main(argc, argv, envp)
	nop
	li	a0, Init			# done, so call prom
	j	a0

/* dummy routine for gcc2 */
	.globl	__main
__main:
	j	ra
	nop

	.globl	Boot_Transfer
Boot_Transfer:
	mtc0	zero, MACH_COP_0_STATUS_REG	# Disable interrupts
	li	sp, MACH_CODE_START - START_FRAME
	la	gp, _gp
	sw	zero, START_FRAME - 4(sp)	# Zero out old ra for debugger
	sw	zero, START_FRAME - 8(sp)	# Zero out old fp for debugger
	jal	a3				# Jump to routine
	nop
