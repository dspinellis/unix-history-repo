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
_sccsid:.asciz	"@(#)gets.s	5.3 (Berkeley) %G%"
#endif /* LIBC_SCCS */

/*
 * char *gets(s);
 * char *s;
 *
 * argument: a target string
 * side effects: reads bytes up to and including a newline from the
 *	standard input into the target string and replaces the newline
 *	with a null to null-terminate the string.
 * result: the target string if successful, 0 otherwise.
 */

#include "DEFS.h"

#define		NL	0xa

ENTRY(gets, R11|R10)

#define		S	r11
	movl	4(ap),S
#define		IPTR	r10
#define		_CNT
#define		_PTR	4
#define		_BASE	8
#define		_BUFSIZ	12
#define		_FLAG	16
	movab	__iob,IPTR

#define		OLD_S	4(ap)

	/*
	 * If no characters, call _filbuf() to get some.
	 */
	tstl	_CNT(IPTR)
	jgtr	Lscan

Lloop:
	pushl	IPTR
	calls	$1,__filbuf
	tstl	r0			/* What did _filbuf() return? */
	jlss	Leof
	cmpb	r0,$NL
	jneq	1f
	clrb	(S)
	jbr	Lret
1:
	movb	r0,(S)+			/* Save the returned character */
	tstl	_BASE(IPTR)		/* Is input buffered? */
	jeql	Lloop

	/*
	 * Look for a newline in the buffer.
	 */
Lscan:
	locc	$NL,_CNT(IPTR),*_PTR(IPTR)
	jeql	Lagain

	/*
	 * Success -- copy the data and return.
	 */
	subl3	r0,_CNT(IPTR),r2
	subl2	r2,_CNT(IPTR)
	movc3	r2,*_PTR(IPTR),(S)	/* Copy the data */
	clrb	(r3)
	movl	r1,_PTR(IPTR)
	decl	_CNT(IPTR)		/* Skip the newline */
	incl	_PTR(IPTR)

	/*
	 * Normal return.
	 */
Lret:
	movl	OLD_S,r0
	ret

	/*
	 * If we run out of characters, copy the buffer and loop.
	 */
Lagain:
	movc3	_CNT(IPTR),*_PTR(IPTR),(S)	/* Copy the data */
	movl	r3,S
	movl	_BASE(IPTR),_PTR(IPTR)		/* Reset stdio */
	clrl	_CNT(IPTR)
	jbr	Lloop

	/*
	 * End of file?  Check to see if we copied any data.
	 */
Leof:
	cmpl	S,OLD_S
	jeql	Lerror
	clrb	(S)
	jbr	Lret

	/*
	 * Error/eof return -- null pointer.
	 */
Lerror:
	clrl	r0
	ret
