/*
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66.
 *
 * Copyright (c) 1992 The Regents of the University of California.
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
 *	California, Lawrence Berkeley Laboratory.
 * 4. The name of the University may not be used to endorse or promote
 *    products derived from this software without specific prior
 *    written permission.
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

#include <machine/asm_linkage.h>
#define LOCORE 1
#include <machine/clock.h>
#include "assym.s"			/* to get CNTR_COUNTER10 */

	.seg	"text"
 .asciz "microtime.s,v 3.1 1993/07/06 01:10:10 jbj Exp";
 .asciz "Copyright (c) 1992 Regents of the University of California";
	.align	4
/*
 * void microtime(struct timeval *tv)
 *
 * 'microtime' code taken from LBL's sparc bsd:  We don't
 * need to spl (so this routine can be a leaf routine) and
 * we don't keep a 'last' timeval (there can't be two calls
 * to this routine in a microsecond).  This seems to be
 * about 20 times faster than the Sun code on an SS-2. - vj
 *
 * Read time values from slowest-changing to fastest-changing,
 * then re-read out to slowest.  If the values read before
 * the innermost match those read after, the innermost value
 * is consistent with the outer values.  If not, it may not
 * be and we must retry.  Typically this loop runs only once;
 * occasionally it runs twice, and only rarely does it run longer.
 */
	.globl	_time
	.globl	_tick

	ENTRY(microtime)
	ALTENTRY(uniqtime)

	sethi	%hi(_time), %g2
	sethi	%hi(COUNTER_ADDR+CNTR_COUNTER10), %g3
again:
	ldd	[%g2+%lo(_time)], %o2		! time.tv_sec & time.tv_usec
	ld	[%g3+%lo(COUNTER_ADDR+CNTR_COUNTER10)], %o4 ! usec counter
	ldd	[%g2+%lo(_time)], %g4		! see if time values changed
	cmp	%g4, %o2
	bne	again				! if time.tv_sec changed
	 cmp	%g5, %o3
	bne	again				! if time.tv_usec changed
	 tst	%o4

	bpos	notAtLimit
	 srl	%o4, CTR_USEC_SHIFT, %o4	! convert counter to usec
	sethi	%hi(_tick), %g4			! bump usec by 1 tick
	ld	[%g4+%lo(_tick)], %o1
	set	CTR_USEC_MASK >> CTR_USEC_SHIFT, %g5
	add	%o1, %o3, %o3
	and	%o4, %g5, %o4
notAtLimit:
	add	%o4, %o3, %o3
	set	1000000, %g5			! normalize usec value
	cmp	%o3, %g5
	bl,a	noOflo
	 st	%o2, [%o0]			! (should be able to std here)
	add	%o2, 1, %o2
	sub	%o3, %g5, %o3
	st	%o2, [%o0]			! (should be able to std here)
noOflo:
	retl
	 st	%o3, [%o0+4]
