/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)fgets.s	5.5 (Berkeley) 6/27/88"
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
