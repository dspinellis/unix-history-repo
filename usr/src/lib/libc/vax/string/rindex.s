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
	.asciz "@(#)rindex.s	8.1 (Berkeley) 6/4/93"
#endif /* LIBC_SCCS and not lint */

/*
 * Find the last occurence of c in the string cp.
 * Return pointer to match or null pointer.
 *
 * char *
 * rindex(cp, c)
 *	char *cp, c;
 */
#include "DEFS.h"

ENTRY(rindex, 0)
	movq	4(ap),r1	# r1 = cp; r2 = c
	tstl	r2		# check for special case c == '\0'
	bneq	2f
1:
	locc	$0,$65535,(r1)	# just find end of string
	beql	1b		# still looking
	movl	r1,r0		# found it
	ret
2:
	moval	tbl,r3		# r3 = address of table
	bbss	$0,(r3),5f	# insure not reentering
	movab	(r3)[r2],r5	# table entry for c
	incb	(r5)
	clrl	r4		# last found
3:
	scanc	$65535,(r1),(r3),$1	# look for c or '\0'
	beql	3b		# keep looking
	tstb	(r1)		# if have found '\0'
	beql	4f		#    we are done
	movl	r1,r4		# save most recently found
	incl	r1		# skip over character
	jbr	3b		# keep looking
4:
	movl	r4,r0		# return last found (if any)
	clrb	(r5)		# clean up table
	clrb	(r3)
	ret

	.data
tbl:	.space	256
	.text

/*
 * Reentrant, but slower version of rindex
 */
5:
	movl	r1,r3
	clrl	r4		# r4 = pointer to last match
6:
	locc	$0,$65535,(r3)	# look for '\0'
	bneq	8f
	decw	r0		# r0 = 65535
1:
	locc	r2,r0,(r3)	# look for c
	bneq	7f
	movl	r1,r3		# reset pointer and ...
	jbr	6b		# ... try again
7:
	movl	r1,r4		# stash pointer ...
	addl3	$1,r1,r3	# ... skip over match and ...
	decl	r0		# ... decrement count
	jbr	6b		# ... try again
8:
	subl3	r3,r1,r0	# length of short block
	incl	r0		# +1 for '\0'
9:
	locc	r2,r0,(r3)	# look for c
	beql	0f
	movl	r1,r4		# stash pointer ...
	addl3	$1,r1,r3	# ... skip over match ...
	decl	r0		# ... adjust count and ...
	jbr	9b		# ... try again
0:
	movl	r4,r0		# return stashed pointer
	ret
