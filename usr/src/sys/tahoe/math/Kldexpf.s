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
 *	@(#)Kldexpf.s	7.1 (Berkeley) 12/6/90
 */

#include "../tahoe/SYS.h"
#include "../math/fp.h"
#include "../math/Kfp.h"

/* @(*)Kldexpf.s	4.2 (Berkeley) 12/21/80
 *	Tahoe 		2/2/83
 *
 * float Kldexpf (op_most, op_least, exp, hfs)
 *
 * Ldexp returns value*2**exp, if that result is in range.
 * If underflow occurs, it returns zero.  If overflow occurs,
 * it returns a value of appropriate sign and largest
 * possible magnitude.  In case of either overflow or underflow,
 * the external int "errno" is set to ERANGE.  Note that errno is
 * not modified if no error occurs, so if you intend to test it
 * after you use Kldexpf, you had better set it to something
 * other than ERANGE first (zero is a reasonable value to use).
 */

	.text
ENTRY(Kldexpf, R2)
	movl	4(fp),r0	/* Fetch "value" */
	movl	8(fp),r1

	andl3	$EXPMASK,r0,r2	/* r2 := shifted biased exponent */
	jeql	ld1		/* If it's zero, we're done */
	shar	$EXPSHIFT,r2,r2	/* shift to get value of exponent  */

	addl2	12(fp),r2	/* r2 := new biased exponent */
	jleq	under		/* if it's <= 0, we have an underflow */
	cmpl	r2,$256		/* Otherwise check if it's too big */
	jgeq	over		/* jump if overflow */
/*
 *	Construct the result and return
 */
	andl2	$0!EXPMASK,r0	/* clear old exponent */
	shal 	$EXPSHIFT,r2,r2	/* Put the exponent back in the result */
	orl2	r2,r0
ld1:	ret
/*
 *	Underflow
 */
under:	clrl	r0		/* Result is zero */
	clrl	r1
	orl2	$HFS_UNDF,*16(fp)
	jmp	err		/* Join general error code */
/*
 *	Overflow
 */
over:	movl	huge0,r0	/* Largest possible floating magnitude */
	movl	huge1,r1
	orl2	$HFS_OVF,*16(fp)
	orl2	$SIGNBIT,r0	/* If arg < 0, make result negative */

err:	orl2	$HFS_RANGE,*16(fp)	/* Indicate range error */
	ret

	.data
huge0:	.long	0x7fffffff
huge1:	.long	0xffffffff
