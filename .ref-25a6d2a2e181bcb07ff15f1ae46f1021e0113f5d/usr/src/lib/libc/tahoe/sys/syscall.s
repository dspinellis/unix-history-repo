/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if defined(SYSLIBC_SCCS) && !defined(lint)
	.asciz "@(#)syscall.s	5.3 (Berkeley) %G%"
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
