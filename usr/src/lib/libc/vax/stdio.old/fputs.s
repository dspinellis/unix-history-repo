/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific written prior permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#ifdef LIBC_SCCS
_sccsid:.asciz	"@(#)fputs.s	5.5 (Berkeley) %G%"
#endif /* LIBC_SCCS */

/*
 * fputs(s, iop);
 * char *s;
 * FILE *iop;
 *
 * arguments: a source string and a file pointer.
 * side effects: writes to the file indicated by iop using the data in
 *	the null-terminated source string.
 * result: technically void; for compatibility we return 0 for the null
 *	string, non-zero otherwise.  We return zero for errors too.
 */

#include "DEFS.h"

#define		NBF	04
#define		LBF	0200

#define		NL	012

ENTRY(fputs, R11|R10|R9)

#define		S	r11
	movl	4(ap),S
#define		IOP	r10
#define		_CNT
#define		_PTR	4
#define		_BASE	8
#define		_BUFSIZ	12
#define		_FLAG	16
	movl	8(ap),IOP

#define		UNBUF	-4(fp)

#define		COUNT	r9

	/*
	 * For compatibility (sigh).
	 */
	tstb	(S)
	jeql	Lerror

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
	pushl	IOP			/* Get _flsbuf() to do the work */
	pushl	$0
	calls	$2,__flsbuf
	tstl	r0
	jlss	Lerror
	incl	_CNT(IOP)		/* Unput the char we sent */
	decl	_PTR(IOP)
2:

	/*
	 * Search for the terminating null.
	 * We only need to look at _BUFSIZ bytes or less on each pass.
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
Lfixup:
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
	jbr	Lfixup

	/*
	 * Bomb out.  Return 0 (why not? that's what the old one did).
	 */
Lerror:
	clrl	r0
	ret
