/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(SYSLIBC_SCCS) && !defined(lint)
	.asciz "@(#)syscall.s	5.4 (Berkeley) %G%"
#endif /* SYSLIBC_SCCS and not lint */

#include "SYS.h"

ENTRY(syscall)
	pushl	4(fp)		# syscall number
	movl	fp,r0		# point to the arg list
	movl	-4(fp),r1	# (arg_count + 1) (bytes) | mask
	andl2	$0xFFFF,r1	# clear the mask bits
	shrl	$2,r1,r1	# convert to words
	subl2	$2,r1		# don't count the first arg
1:
	addl2	$4,r0		# point to the next arg
	movl	4(r0),(r0)	# move an arg down
	decl	r1		# count it
	jgtr	1b		# any more?
	movl	(sp)+,r0	# no, get the syscall number back
	kcall	r0
	jcs	1f
	ret
1:
	jmp	cerror
