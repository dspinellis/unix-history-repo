/*
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)crt0.s	8.2 (Berkeley) 3/21/94
 */

#include <machine/regdef.h>
#include <machine/machAsmDefs.h>

	.comm	environ, 4
	.data
	.align	2
	.globl	__progname
__progname:
	.word	$L1
$L1:
	.word	0		# null string plus padding
	.text

NON_LEAF(start, 24, ra)
	.set	noreorder
	lw	s0, 0(sp)	# get argc from stack
	addu	s1, sp, 4	# get pointer to argv
	addu	s2, s1, 4	# skip null pointer on stack
	sll	v0, s0, 2	# add number of argv pointers
	addu	s2, s2, v0	# final pointer to environment list
	sw	s2, environ	# save environment pointer
	subu	sp, sp, 24	# allocate standard frame
	.mask	0x80000000, -4
	sw	zero, 20(sp)	# clear return address for debugging
#ifdef MCRT0
eprol:
	la	a0, eprol
	la	a1, etext
	jal	monstartup	# monstartup(eprol, etext);
	nop
	la	a0, _mcleanup
	jal	atexit		# atext(_mcleanup);
	nop
	sw	zero, errno
#endif
	lw	a0, 0(s1)	# a0 = argv[0];
	nop
	beq	a0, zero, 2f	# skip if a0 == NULL
	move	s3, a0		# save argv[0]
	jal	strrchr
	li	a1, 0x2f	# a1 = '/'
	bne	v0, zero, 1f	# if slash found
	addu	v0, v0, 1
	move	v0, s3		# v0 = argv[0];
1:
	sw	v0, __progname
2:
	move	a0, s0
	move	a1, s1
	jal	main		# v0 = main(argc, argv, env);
	move	a2, s2
	jal	exit		# exit(v0);
	move	a0, v0
	break	0
	.set	reorder
END(start)

#ifndef MCRT0
LEAF(moncontrol)
	j	ra
END(moncontrol)

LEAF(_mcount)
	.set	noreorder
	.set	noat
	addu	sp, sp, 8	# undo push
	j	ra
	move	ra, AT
	.set	at
	.set	reorder
END(_mcount)
#endif
