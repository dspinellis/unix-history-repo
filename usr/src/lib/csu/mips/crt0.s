/*
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)crt0.s	5.3 (Berkeley) %G%
 */

#include <machine/regdef.h>
#include <machine/machAsmDefs.h>

	.comm	environ, 4

NON_LEAF(__start, 24, ra)
	li	v0, 45		# illegal ULTRIX system call to switch to BSD
	syscall			#   system call numbers.
	lw	a0, 0(sp)	# get argc from stack
	la	gp, _gp		# init gp pointer
	addu	a1, sp, 4	# get pointer to argv
	addu	a2, a1, 4	# skip null pointer on stack
	sll	v0, a0, 2	# add number of argv pointers
	addu	a2, a2, v0	# final pointer to environment list
	sw	a2, environ	# save environment pointer
	subu	sp, sp, 24	# allocate standard frame
	.mask	0x80000000, -4
	sw	zero, 20(sp)	# clear return address for debugging
	jal	main		# v0 = main(argc, argv, env);
	move	a0, v0
	jal	exit		# exit(v0);
	break	0
END(__start)

LEAF(moncontrol)
	j	ra
END(moncontrol)

LEAF(_mcount)
	j	ra
END(_mcount)
