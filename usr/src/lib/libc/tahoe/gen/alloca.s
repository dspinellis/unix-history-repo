/*
 * Copyright (c) 1988, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)alloca.s	8.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

/* like alloc, but automatic automatic free in return */

#include "DEFS.h"

ENTRY(alloca, 0)
	moval	(sp),r0		# current sp
	subl2	4(fp),r0	# allocation size
	andl2	$0xfffffffc,r0	# allignment
	movl	-8(fp),r1	# old pc
	movl	(fp),fp		# old fp
	addl2	$4*4,r0		# reuse space of mscp
	movl	r0,sp		# new sp
	jmp 	(r1)		# funny return
