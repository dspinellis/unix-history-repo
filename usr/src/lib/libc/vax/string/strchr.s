/*
 * Copyright (c) 1988, 1993
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
	.asciz "@(#)strchr.s	8.1 (Berkeley) 6/4/93"
#endif /* LIBC_SCCS and not lint */

/*
 * Find the first occurence of c in the string cp.
 * Return pointer to match or null pointer.
 *
 * char *
 * strchr(cp, c)
 *	char *cp, c;
 */
#include "DEFS.h"

	.lcomm	tbl,256

ENTRY(strchr, 0)
	movzwl	$65535,r4	/* handy constant */
	movq	4(ap),r1	/* r1 = cp; r2 = c */
	movzbl	r2,r2
	beql	Lzero		/* special case for c == '\0' */

/*
 * Fancy scanc version.  Alas, it is not reentrant.
 */
	movab	tbl,r3		/* r3 = base of table */
	bbss	$0,(r3),Lreent	/* ensure not reentering */
	movab	(r3)[r2],r5	
	incb	(r5)		/* mark both '\0' and c */
0:
	scanc	r4,(r1),(r3),$1	/* look for c or '\0' */
	beql	0b		/* still looking */
	movl	r1,r0		/* return whatever we found */
	tstb	(r0)
	bneq	1f		#	unless it was '\0':
	clrl	r0		#	then return NULL
1:
	clrb	(r5)		/* clean up table */
	clrb	(r3)
	ret

/*
 * Special case for \0.
 */
Lzero:
	locc	r2,r4,(r1)	/* just find end of string */
	beql	Lzero		/* still looking */
	movl	r1,r0		/* found it */
	ret

/*
 * Slower reentrant version is two two-step searches.  The first
 * phase runs until we know where the string ends; it locates the
 * first occurrence of c within a 65535-byte block.  If we find
 * the end of the string first, we switch to the second phase,
 * were we look only up to the known end of string.
 */
Lreent:
0:	/* first phase */
	movl	r1,r3
	locc	$0,r4,(r3)	/* look for '\0' */
	bneq	1f
	locc	r2,r4,(r3)	/* look for c */
	beql	0b		/* not found: reset pointer and loop */
	movl	r1,r0		/* found: return it */
	ret
1:	/* second phase */
	subl3	r3,r1,r0	/* length of short block */
	locc	r2,r0,(r3)	/* look for c */
	beql	2f		/* not found: return NULL */
	movl	r1,r0
2:	ret
