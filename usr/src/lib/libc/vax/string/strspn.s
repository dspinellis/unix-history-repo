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
	.asciz "@(#)strspn.s	8.1 (Berkeley) 6/4/93"
#endif /* LIBC_SCCS and not lint */

/*
 * Span the string s2 (skip characters that are in s2).
 * Return the number of characters in s1 that were skipped.
 *
 * size_t
 * strspn(s1, s2)
 *	const char *s1, *s2;
 */
#include "DEFS.h"

ENTRY(strspn, 0)
	subl2	$32,sp		/* make 256 bit table */
	movc5	$0,(sp),$0,$32,(sp)
	movq	4(ap),r1	/* r1 = s1, r2 = s2 */

	/* turn on bit for each character in s2, including '\0' */
1:
	movzbl	(r2)+,r0
	bbss	r0,(sp),1b
	bneq	1b

	/* now clear bit for '\0' */
	/* (this is easier than avoiding setting it in the first place) */
	bicb2	$1,(sp)		/* stop at '\0' */
	movl	r1,r0		/* r0 = s (current pos in s1) */

	/* look for a character that is not in s2 */
2:
	movzbl	(r0)+,r2	/* c = *s++ */
	bbs	r2,(sp),2b	/* loop while c is in table */
	decl	r0		/* s-- */
	subl2	r1,r0		/* r0 = s - s1 = count */
	ret
