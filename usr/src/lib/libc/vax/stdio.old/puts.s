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
	.asciz "@(#)puts.s	5.7 (Berkeley) 6/1/90"
#endif /* LIBC_SCCS and not lint */

/*
 * puts(s);
 * char *s;
 *
 * argument: a source string.
 * side effects: writes to the standard output using the data in
 *	the null-terminated source string; a newline is appended.
 * result: technically void; for compatibility we return 0 for the null
 *	string, non-zero otherwise.  We return zero for errors too.
 */

#include "DEFS.h"

#define		NBF	04
#define		LBF	0200

#define		NL	012

ENTRY(puts, R11|R10|R9)

#define		S	r11
	movl	4(ap),S
#define		IOP	r10
#define		_CNT
#define		_PTR	4
#define		_BASE	8
#define		_BUFSIZ	12
#define		_FLAG	16
	movab	__iob+20,IOP

#define		UNBUF	-4(fp)

#define		COUNT	r9

	/*
	 * For unbuffered I/O, line buffer the output line.
	 * Ugly but fast -- and doesn't CURRENTLY break anything (sigh).
	 */
	movab	-1028(sp),sp
	bicw3	$~NBF,_FLAG(IOP),UNBUF
	jeql	1f

	bicw2	$NBF,_FLAG(IOP)		/* Clear no-buffering flag */
	movl	sp,_BASE(IOP)		/* Create a buffer */
	movl	sp,_PTR(IOP)
	cvtwl	$1024,_BUFSIZ(IOP)
	jbr	2f

1:
	tstl	_CNT(IOP)		/* Has a buffer been allocated? */
	jgtr	2f
	pushl	IOP			/* Get _flsbuf() to make one */
	pushl	$0
	calls	$2,__flsbuf
	tstl	r0
	jlss	Lerror
	incl	_CNT(IOP)		/* Unput the char we sent */
	decl	_PTR(IOP)
2:

	/*
	 * Search for the terminating null.
	 */
Lloop:
	addl3	_BASE(IOP),_BUFSIZ(IOP),COUNT	/* How many bytes? */
	subl2	_PTR(IOP),COUNT
	locc	$0,COUNT,(S)		/* Look for a null */
	jeql	Lagain

	subl2	r0,COUNT		/* Copy the data */
	movc3	COUNT,(S),*_PTR(IOP)
	movl	r3,_PTR(IOP)		/* Fix up IOP */
	subl2	COUNT,_CNT(IOP)

Lnl:
	movb	$NL,*_PTR(IOP)		/* Append a newline */
	incl	_PTR(IOP)
	decl	_CNT(IOP)

	bitw	$LBF,_FLAG(IOP)		/* If line buffered... */
	jneq	1f
	tstw	UNBUF			/* or unbuffered... */
	jneq	1f
	tstl	_CNT(IOP)		/* or a full buffer... */
	jgtr	2f
1:
	pushl	IOP			/* ... flush the buffer */
	calls	$1,_fflush
	tstl	r0
	jlss	Lerror
2:

	/*
	 * Fix up buffering again.
	 */
	tstw	UNBUF
	jeql	1f
	bisw2	$NBF,_FLAG(IOP)		/* Reset flag */
	clrl	_BASE(IOP)		/* Clear data structure */
	clrl	_BUFSIZ(IOP)
	clrl	_CNT(IOP)
1:
	cvtbl	$NL,r0			/* Compatibility hack */
	ret

	/*
	 * We didn't find the null -- loop.
	 */
Lagain:
	movc3	COUNT,(S),*_PTR(IOP)	/* Copy the data */
	movl	r1,S
	movl	r3,_PTR(IOP)		/* Fix up IOP */
	subl2	COUNT,_CNT(IOP)
	pushl	IOP			/* The buffer is full -- flush it */
	calls	$1,_fflush
	tstl	r0
	jlss	Lerror
	tstb	(S)			/* More data? */
	jneq	Lloop
	jbr	Lnl

	/*
	 * Bomb out.  Return 0 (why not? that's what the old one did).
	 */
Lerror:
	clrl	r0
	ret
