/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.
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
	.asciz "@(#)fgets.s	5.6 (Berkeley) 6/1/90"
#endif /* LIBC_SCCS and not lint */

/*
 * char *fgets(s, n, iptr);
 * char *s;
 * int n;
 * FILE *iptr;
 *
 * arguments: a target string, a length, and a file pointer.
 * side effects: reads up to and including a newline, or up to n-1 bytes,
 *	whichever is less, from the file indicated by iptr into the target
 *	string and null terminates.
 * result: the target string if successful, 0 otherwise.
 */

#include "DEFS.h"

#define		NL	0xa

ENTRY(fgets, R11|R10|R9)

#define		OLD_S	4(ap)
#define		S	r11
	movl	OLD_S,S

#define		N	8(ap)

#define		IPTR	r10
#define		_CNT
#define		_PTR	4
#define		_BASE	8
	movl	12(ap),IPTR

#define		COUNT	r9

	/*
	 * Sanity check -- is the buffer big enough?
	 */
	cmpl	N,$1
	jleq	Lerror

	subl3	$1,N,COUNT		/* We scan at most COUNT chars */

	/*
	 * If no characters, call _filbuf() to get some.
	 */
	tstl	_CNT(IPTR)
	jgtr	Lscan

Lloop:
	pushl	IPTR
	calls	$1,__filbuf
	tstl	r0
	jlss	Leof
	movb	r0,(S)+			/* Save the returned character */
	decl	N
	decl	COUNT
	jleq	1f
	cmpb	r0,$NL			/* If it was a newline, we're done */
	jneq	2f
1:
	clrb	(S)
	jbr	Lret
2:
	tstl	_BASE(IPTR)		/* Is the input buffered? */
	jeql	Lloop			/* If not, loop inefficiently */

	/*
	 * Look for a newline in the buffer.
	 */
Lscan:
	cmpl	_CNT(IPTR),COUNT	/* Is buffer bigger than N-1? */
	jgeq	1f
	movl	_CNT(IPTR),COUNT	/* If not, don't read off the end */
1:
	locc	$NL,COUNT,*_PTR(IPTR)	/* Scan the buffer */
	jeql	Lagain

	/*
	 * Success -- copy the data and return.
	 */
	decl	r0			/* How many characters did we read? */
	subl2	r0,COUNT
	movc3	COUNT,*_PTR(IPTR),(S)	/* Copy the data */
	clrb	(r3)
	subl2	COUNT,_CNT(IPTR)	/* Fix up the I/O buffer */
	movl	r1,_PTR(IPTR)

Lret:
	movl	OLD_S,r0
	ret

	/*
	 * If we run out of characters, copy the buffer and loop if needed.
	 */
Lagain:
	movc3	COUNT,*_PTR(IPTR),(S)	/* Copy the data */
	subl2	COUNT,_CNT(IPTR)	/* Adjust the buffers and counts */
	movl	r1,_PTR(IPTR)
	subl2	COUNT,N
	movl	r3,S
	subl3	$1,N,COUNT
	jgtr	Lloop

	/*
	 * End of file?  Check to see if we copied any data.
	 */
Leof:
	cmpl	S,OLD_S
	jeql	Lerror
	clrb	(S)
	jbr	Lret

	/*
	 * Error return -- null pointer.
	 */
Lerror:
	clrl	r0
	ret
