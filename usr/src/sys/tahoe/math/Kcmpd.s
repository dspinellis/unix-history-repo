/*-
 * Copyright (c) 1985 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
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
 *
 *	@(#)Kcmpd.s	7.1 (Berkeley) 12/6/90
 */

#include "../tahoe/SYS.h"

/*
 * cmpd(hi1, lo1, hi2, lo2)
 *	register hi1, hi2;
 *	register unsigned lo1, lo2;
 *{
 *	if(hi1 < 0) {
 *		hi1 ^= 0x80000000;
 *		if(lo1) {
 *			lo1 = -lo1;
 *			hi1 = ~hi1;
 *		} else
 *			hi1 = -hi1;
 *	}
 *	if(hi2 < 0) {
 *		hi2 ^= 0x80000000;
 *		if(lo2) {
 *			lo2 = -lo2;
 *			hi2 = ~hi2;
 *		} else
 *			hi2 = -hi2;
 *	}
 *	if(hi1 != hi2)
 *		return(hi1>hi2 ? 1 : -1);
 *	if(lo1 != lo2)
 *		return(lo1>lo2 ? 1 : -1);
 *	return(0);
 *}
 */
	.text
ENTRY(Kcmpd, 0)
	movl	8(fp),r3
	movl	12(fp),r4
	movl	16(fp),r2
	movl	4(fp),r5
	jgeq	1f
	xorl2	$0x80000000,r5
	tstl	r3
	jeql	2f
	mnegl	r3,r3
	mcoml	r5,r5
	jbr	1f
2:
	mnegl	r5,r5
1:
	tstl	r4
	jgeq	1f
	xorl2	$0x80000000,r4
	tstl	r2
	jeql	2f
	mnegl	r2,r2
	mcoml	r4,r4
	jbr	1f
2:
	mnegl	r4,r4
1:
	cmpl	r5,r4
	jeql	1f
	jleq	2f
	movl	$1,r0
	ret
2:
	mnegl	$1,r0
	ret
1:
	cmpl	r3,r2
	jeql	1f
	jlequ	2f
	movl	$1,r0
	ret
2:
	mnegl	$1,r0
	ret
1:
	clrl	r0
	ret
