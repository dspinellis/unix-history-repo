/*-
 * Copyright (c) 1990, 1993
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
	.asciz "@(#)memcmp.s	8.1 (Berkeley) 6/4/93"
#endif /* LIBC_SCCS and not lint */

/* int memcmp(s1, s2, n) */

#include "DEFS.h"

ENTRY(memcmp, 0)
	movl	4(ap),r1	/* r1 = s1 */
	movq	8(ap),r3	/* r3 = s2; r4 = n */
	movzwl	$65535,r5
0:
	cmpl	r4,r5
	jgtru	3f		/* handle stupid cmpc3 limitation */
	cmpc3	r4,(r1),(r3)	/* compare */
	beql	2f		/* done if same (r0 = 0) */
1:
	movzbl	(r1),r0
	movzbl	(r3),r2
	subl2	r2,r0		/* return *s1 - *s2; s1,s2 unsigned chars */
2:
	ret
3:
	subl2	r5,r4		/* do 64K; adjust count */
	cmpc3	r5,(r1),(r3)
	jeql	0b		/* loop if same */
	jbr	1b
