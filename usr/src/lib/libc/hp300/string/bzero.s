/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
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
	.asciz "@(#)bzero.s	8.1 (Berkeley) 6/4/93"
#endif /* LIBC_SCCS and not lint */

#include "DEFS.h"

/*
 * This is probably not the best we can do, but it is still much
 * faster than the C version in the portable gen directory.
 *
 * Things that might help:
 *	- unroll the longword loop (might not be good for a 68020)
 *	- longword, as opposed to word, align when possible (only on the 68020)
 *	- use nested DBcc instructions or use one and limit size to 64K
 */
ENTRY(bzero)
	movl	sp@(4),a0	/* destination */
	movl	sp@(8),d0	/* count */
	jeq	bzdone		/* nothing to do */
	movl	a0,d1
	btst	#0,d1		/* address odd? */
	jeq	bzeven		/* no, skip alignment */
	clrb	a0@+		/* yes, clear a byte */
	subql	#1,d0		/* adjust count */
	jeq	bzdone		/* if zero, all done */
bzeven:
	movl	d0,d1
	lsrl	#2,d1		/* convert to longword count */
	jeq	bzbloop		/* no longwords, skip loop */
bzlloop:
	clrl	a0@+		/* clear a longword */
	subql	#1,d1		/* adjust count */
	jne	bzlloop		/* still more, keep going */
	andl	#3,d0		/* what remains */
	jeq	bzdone		/* nothing, all done */
bzbloop:
	clrb	a0@+		/* clear a byte */
	subql	#1,d0		/* adjust count */
	jne	bzbloop		/* still more, keep going */
bzdone:
	rts
