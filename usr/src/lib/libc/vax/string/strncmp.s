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
	.asciz "@(#)strncmp.s	8.1 (Berkeley) 6/4/93"
#endif /* LIBC_SCCS and not lint */

/*
 * Compare at most n characters of string
 * s1 lexicographically to string s2.
 * Return:
 *	0	s1 == s2
 *	> 0	s1 > s2
 *	< 0	s2 < s2
 *
 * strncmp(s1, s2, n)
 *	char *s1, *s2;
 *	int n;
 */
#include "DEFS.h"

ENTRY(strncmp, 0)
	movl	4(ap),r1	# r1 = s1
	movq	8(ap),r3	# r3 = s2; r4 = n
1:
	clrl	r5		# calculate min bytes to next page boundry
	subb3	r1,$255,r5	# r5 = (bytes - 1) to end of page for s1
	subb3	r3,$255,r0	# r0 = (bytes - 1) to end of page for s2
	cmpb	r0,r5		# r5 = min(r0, r5);
	bgtru	2f
	movb	r0,r5
2:
	incl	r5		# r5 = min bytes to next page boundry
	cmpl	r4,r5		# r5 = min(n, r5);
	bgeq	3f
	movl	r4,r5
3:
	cmpc3	r5,(r1),(r3)	# compare strings
	bneq	4f
	subl2	r5,r4		# check for end of comparison
	beql	5f
	subl2	r5,r1		# check if found null yet
	locc	$0,r5,(r1)
	beql	1b		# not yet done, continue checking
	subl2	r0,r3
	mnegb	(r3),r0		# r0 = '\0' - *s2
	cvtbl	r0,r0
	ret
4:
	subl2	r0,r5		# check for null in matching string
	subl2	r5,r1
	locc	$0,r5,(r1)
	bneq	5f
	subb3	(r3),(r1),r0	# r0 = *s1 - *s2
	cvtbl	r0,r0
	ret
5:
	clrl	r0		# both the same to null
	ret
