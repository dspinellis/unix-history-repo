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
	.asciz "@(#)bcopy.s	8.1 (Berkeley) 6/4/93"
#endif /* LIBC_SCCS and not lint */

#include "DEFS.h"

/*
 * This is probably not the best we can do, but it is still 2-10 times
 * faster than the C version in the portable gen directory.
 *
 * Things that might help:
 *	- unroll the longword copy loop (might not be good for a 68020)
 *	- longword align when possible (only on the 68020)
 *	- use nested DBcc instructions or use one and limit size to 64K
 */
ENTRY(bcopy)
	movl	sp@(12),d1	/* check count */
	jle	bcdone		/* <= 0, don't do anything */
	movl	sp@(4),a0	/* src address */
	movl	sp@(8),a1	/* dest address */
	cmpl	a1,a0		/* src after dest? */
	jlt	bcback		/* yes, must copy backwards */
	movl	a0,d0
	btst	#0,d0		/* src address odd? */
	jeq	bcfeven		/* no, skip alignment */
	movb	a0@+,a1@+	/* yes, copy a byte */
	subql	#1,d1		/* adjust count */
	jeq	bcdone		/* count 0, all done  */
bcfeven:
	movl	a1,d0
	btst	#0,d0		/* dest address odd? */
	jne	bcfbloop	/* yes, no hope for alignment, copy bytes */
	movl	d1,d0		/* no, both even */
	lsrl	#2,d0		/* convert count to longword count */
	jeq	bcfbloop	/* count 0, skip longword loop */
bcflloop:
	movl	a0@+,a1@+	/* copy a longword */
	subql	#1,d0		/* adjust count */
	jne	bcflloop	/* still more, keep copying */
	andl	#3,d1		/* what remains */
	jeq	bcdone		/* nothing, all done */
bcfbloop:
	movb	a0@+,a1@+	/* copy a byte */
	subql	#1,d1		/* adjust count */
	jne	bcfbloop	/* still more, keep going */
bcdone:
	rts
bcback:
	addl	d1,a0		/* src pointer to end */
	addl	d1,a1		/* dest pointer to end */
	movl	a0,d0
	btst	#0,d0		/* src address odd? */
	jeq	bcbeven		/* no, skip alignment */
	movb	a0@-,a1@-	/* yes, copy a byte */
	subql	#1,d1		/* adjust count */
	jeq	bcdone		/* count 0, all done  */
bcbeven:
	movl	a1,d0
	btst	#0,d0		/* dest address odd? */
	jne	bcbbloop	/* yes, no hope for alignment, copy bytes */
	movl	d1,d0		/* no, both even */
	lsrl	#2,d0		/* convert count to longword count */
	jeq	bcbbloop	/* count 0, skip longword loop */
bcblloop:
	movl	a0@-,a1@-	/* copy a longword */
	subql	#1,d0		/* adjust count */
	jne	bcblloop	/* still more, keep copying */
	andl	#3,d1		/* what remains */
	jeq	bcdone		/* nothing, all done */
bcbbloop:
	movb	a0@-,a1@-	/* copy a byte */
	subql	#1,d1		/* adjust count */
	jne	bcbbloop	/* still more, keep going */
	rts

