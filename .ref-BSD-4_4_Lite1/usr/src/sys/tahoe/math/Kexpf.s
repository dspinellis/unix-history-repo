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
 *	@(#)Kexpf.s	7.1 (Berkeley) 12/6/90
 */

#include "../tahoe/SYS.h"

	.text
ENTRY(Kexpf, R4|R3)
	subl3	$88,fp,sp
	tstl	4(fp)
	jneq	1f
	movl	one,r0
	ret
1:
	lnd	_maxf
	cmpd	4(fp)
	jleq	1f
	clrl	r0
	ret
1:
	cmpd2	4(fp),_maxf
	jleq	1f
	ldd	_HUGE
	cvdf
	stf	r0
	ret
1:	
	pushl	20(fp)		# hfs
	ldd	_log2e; pushd
	ldd	4(fp); pushd
	callf	$24,_Kmuld
	ldd	r0
	std	4(fp)

	pushl	20(fp)		# hfs
	pushl	8(fp)
	pushl	4(fp)
	callf	$16,_Kfloorf
	movl	r0,-88(fp)	# (int)ent from Kfloorf

	cvlf	-88(fp)
	clrl	r1
	stf	r0
	pushl	20(fp)		# hfs
	pushl	r1
	pushl	r0
	ldd	4(fp); pushd
	callf	$24,_Ksubd	# (arg - ent)

	pushl	20(fp)		# hfs
	ldd	half; pushd
	ldd	r0; pushd
	callf	$24,_Ksubd
	ldd	r0; std	-60(fp)	# fract

	pushl	20(fp)		# hfs
	pushd
	pushd
	callf	$24,_Kmuld
	ldd	r0; std	-84(fp)	# xsq

	pushl	20(fp)		# hfs
	pushd			# xsq
	ldd	_p2; pushd
	callf	$24,_Kmuld

	pushl	20(fp)		# hfs
	ldd	_p1; pushd
	ldd	r0; pushd
	callf	$24,_Kaddd

	pushl	20(fp)		# hfs
	ldd	-84(fp); pushd
	ldd	r0; pushd
	callf	$24,_Kmuld

	pushl	20(fp)		# hfs
	ldd	_p0; pushd
	ldd	r0; pushd
	callf	$24,_Kaddd

	pushl	20(fp)		# hfs
	ldd	-60(fp); pushd	# fract
	ldd	r0; pushd
	callf	$24,_Kmuld
	ldd	r0; std	-68(fp)	# temp1

	pushl	20(fp)		# hfs
	ldd	-84(fp); pushd	# xsq
	ldd	_q2; pushd
	callf	$24,_Kmuld

	pushl	20(fp)		# hfs
	ldd	_q1; pushd
	ldd	r0; pushd
	callf	$24,_Kaddd

	pushl	20(fp)		# hfs
	ldd	-84(fp); pushd
	ldd	r0; pushd
	callf	$24,_Kmuld

	pushl	20(fp)		# hfs
	ldd	_q0; pushd
	ldd	r0; pushd
	callf	$24,_Kaddd
	ldd	r0; std	-76(fp)	# temp2

	pushl	20(fp)		# hfs for Kldexpf
	pushl	-88(fp)		# ent

	pushl	20(fp)		# hfs tor temp2+temp1
	ldd	-68(fp); pushd	# temp1
	ldd	-76(fp); pushd	# temp2
	callf	$24,_Kaddd

	pushl	20(fp)		# hfs
	ldd	_sqrt2; pushd
	ldd	r0; pushd	# temp2+temp1
	callf	$24,_Kmuld
	ldd	r0; std	r2		# sqrt2*(temp2+temp1)

	pushl	20(fp)		# hfs
	ldd	-68(fp)
	pushd
	ldd	-76(fp)
	pushd
	callf	$24,_Ksubd	# temp2-temp1

	pushl	20(fp)		# hfs
	ldd	r0
	pushd			# temp2-temp1
	ldd	r2
	pushd
	callf	$24,_Kdivd

	ldd	r0
	pushd
	callf	$20,_Kldexpf

	#pushl	20(fp)		# hfs
	#pushl	$0
	#pushl	r0		# cvlf	r0
	#pushl	$0
	#pushl	$0
	#callf	$24,_Kcvtld

	#ldd	r0
	#cvdf
	#stf	r0
	ret

/* file : Kfloorf.x
*/
	.text
	.globl	_Kfloorf
	.data
	.align	2
L19:	.long	0x40800000, 0x00000000 # .double 1
	.text
	.set	L13,0x0
	.data
	.text
_Kfloorf:
	.word	L13
	subl3	$60,fp,sp
	tstl	4(fp)		# if (d<0.0) {
	jgeq	L17
	lnd	4(fp)
	std	4(fp)		# d = -d;
	pushl	12(fp)		# hfs
	addl3	$4,fp,-(sp)	# &d
	pushl	8(fp)
	pushl	4(fp)
	callf	$20,_Kmodf
	clrl	-60+4(fp)
	movl	r0,-60(fp)	# fract = modf(d,&d);
	tstl	r0		# if (fract != 0.0)
	jeql	L18
	pushl	12(fp)		# hfs
	ldd	L19
	pushd
	ldd	4(fp)
	pushd
	callf	$24,_Kaddd
	ldd	r0
	std	4(fp)		# d +=1;
L18:	lnd	4(fp)
	std	4(fp)		# d = -d;
	jbr	L20
L17:	
	pushl	12(fp)		# hfs
	addl3	$4,fp,-(sp)
	pushl	8(fp)
	pushl	4(fp)
	callf	$20,_Kmodf
L20:	ldd	4(fp)
	cvdl	r0		# for Kexpf call only!
	ret
	
	.data
_p0:	.long	0x45BD3D04, 0x7F734DBD # .double 1513.9067990543389
_p1:	.long	0x42A19DD4, 0x989F60DA # .double 20.202065651286927
_p2:	.long	0x3DBD2E42, 0xAB70BDA9 # .double .023093347753750233
_q0:	.long	0x468881B1, 0x7C3A6529 # .double 4368.2116627275583
_q1:	.long	0x44692F28, 0x7AE89541 # .double 233.18421142748162
_q2:	.long	0x40800000, 0x00000000 # .double 1
_log2e:	.long	0x40B8AA3B, 0x295C17F0 # .double 1.4426950408889634
_sqrt2:	.long	0x40B504F3, 0x33F9DE64 # .double 1.414213562373095
_maxf:	.long	0x471C4000, 0x00000000 # .double 10000
_HUGE:	.long	0x7FFFFFFE, 0xFFFFFFFC # .double 1.701411733192644e+38
one:	.long	0x40800000 # .float 1
half:	.long	0x40000000, 0x00000000 # .double .5
