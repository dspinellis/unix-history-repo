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

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)bcopy.s	5.5 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

/* bcopy(from, to, size) */

#include "DEFS.h"

ENTRY(bcopy, R6)
	movl	4(ap),r1
	movl	8(ap),r3
	movl	12(ap),r6
	cmpl	r1,r3
	bgtr	2f		# normal forward case
	blss	3f		# overlapping, must do backwards
	ret			# equal, nothing to do
1:
	subl2	r0,r6
	movc3	r0,(r1),(r3)
2:
	movzwl	$65535,r0
	cmpl	r6,r0
	jgtr	1b
	movc3	r6,(r1),(r3)
	ret
3:
	addl2	r6,r1
	addl2	r6,r3
	movzwl	$65535,r0
	jbr	5f
4:
	subl2	r0,r6
	subl2	r0,r1
	subl2	r0,r3
	movc3	r0,(r1),(r3)
	movzwl	$65535,r0
	subl2	r0,r1
	subl2	r0,r3
5:
	cmpl	r6,r0
	jgtr	4b
	subl2	r6,r1
	subl2	r6,r3
	movc3	r6,(r1),(r3)
	ret
