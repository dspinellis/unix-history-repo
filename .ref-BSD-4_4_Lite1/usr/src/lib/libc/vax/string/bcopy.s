/*
 * Copyright (c) 1983, 1993
 *	The Regents of the University of California.  All rights reserved.
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
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)bcopy.s	8.1 (Berkeley) 6/4/93"
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
