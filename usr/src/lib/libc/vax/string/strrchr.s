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
	.asciz "@(#)strrchr.s	8.1 (Berkeley) 6/4/93"
#endif /* LIBC_SCCS and not lint */

/*
 * Find the last occurence of c in the string cp.
 * Return pointer to match or null pointer.
 *
 * char *
 * strrchr(cp, c)
 *	char *cp, c;
 */
#include "DEFS.h"

	.lcomm	tbl,256

ENTRY(strrchr, 0)
	movzwl	$65535,r4	/* handy 65535 */
	movq	4(ap),r1	/* r1 = cp; r2 = c */
	movzbl	r2,r2
	beql	Lzero		/* special case for c == '\0' */

	clrl	r5		/* r5 = pointer to last match */

/*
 * Fancy scanc version.  Alas, it is not reentrant.
 */
	movab	tbl,r3		/* r3 = address of table */
	bbss	$0,(r3),Lreent	/* ensure not reentering */
	movab	(r3)[r2],r4
	incb	(r4)		/* mark both '\0' and c */
0:
	scanc	$65535,(r1),(r3),$1	/* look for c or '\0' */
	beql	0b		/* keep looking */
	tstb	(r1)
	beql	1f		/* done if '\0' */
	movab	(r1)+,r5	/* save most recently found, and skip over it */
	jbr	0b		/* keep looking */
1:
	movl	r5,r0		/* return last found (if any) */
	clrb	(r4)		/* clean up table */
	clrb	(r3)
	ret

/*
 * Special case for \0.
 */
Lzero:
	locc	$0,r4,(r1)	/* just find end of string */
	beql	Lzero		/* still looking */
	movl	r1,r0		/* found it */
	ret

/*
 * Slower reentrant version is two two-step searches.  The first
 * phase runs until we know where the string ends; it locates any
 * occurrences of c within a 65535-byte block.  Once we have found
 * the end of the string, we find any further occurrences before
 * that location.
 */
Lreent:
0:	/* first phase */
	movl	r1,r3
	locc	$0,r4,(r3)	/* look for '\0' */
	bneq	1f
	locc	r2,r4,(r3)	/* continue phase 1 search for c */
	beql	0b
	movab	(r1)+,r5	/* found c: save and increment pointer */
	brb	0b		/* and continue */

1:	/* second phase */
	subl3	r3,r1,r0	/* length of short block */
	movl	r3,r1
2:
	locc	r2,r0,(r1)	/* look for c */
	beql	3f		/* skip if not found */
	movab	(r1)+,r5	/* save pointer as before */
	sobgtr	r0,2b		/* adjust count and loop */
3:
	movl	r5,r0		/* return stashed pointer */
	ret
