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
 *	@(#)Klogf.s	7.1 (Berkeley) 12/6/90
 */

#include "../tahoe/SYS.h"

	.text
ENTRY(Klogf, R5|R4|R3|R2)
	subl3	$88,fp,sp
	clrl	8(fp)
	tstl	4(fp)
	jgtr	L53
	movl	small+4,r1
	movl	small,r0
	ret
L53:	pushl	20(fp)			# hfs
	subl3	$88,fp,-(sp)		# &exp
	pushl	8(fp)			# arg
	pushl	4(fp)
	callf	$20,_Kfrexpf
	ldd	r0
					# movl	r1,-56(fp)
	std	-60(fp)			# movl	r0,-60(fp)
	jbr	L55
L2000001:
	pushl	20(fp)			# hfs
	ldd	two			# 2.0
	pushd
	ldd	-60(fp)			# x
	pushd	
	callf	$24,_Kmuld
	ldd	r0
	std	-60(fp)
	subl2	$1,-88(fp)
L55:	cmpd2	-60(fp),half
	jlss	L2000001
	cmpd2	-60(fp),_sqrto2
	jgeq	L59
	pushl	20(fp)			# hfs
	ldd	-60(fp)			# x
	pushd
	ldd	two			# 2.0
	pushd
	callf	$24,_Kmuld
	ldd	r0
	std	-60(fp)
	subl2	$1,-88(fp)
L59:	pushl	20(fp)			# hfs
	ldd	one			# 1.0
	pushd
	ldd	-60(fp)			# x
	pushd
	callf	$24,_Ksubd	
	ldd	r0
	std	r2
	pushl	20(fp)			# hfs
	ldd	one			# 1.0
	pushd
	ldd	-60(fp)			# x
	pushd
	callf	$24,_Kaddd
	ldd	r0
	std	r4
	pushl	20(fp)			# hfs
	ldd	r4			# temp result of x-1
	pushd
	ldd	r2			# temp result of x+1
	pushd
	callf	$24,_Kdivd
	ldd	r0
	std	-68(fp)			# z
	pushl	20(fp)			# hfs
	pushd				# z
	pushd				# z
	callf	$24,_Kmuld
	ldd	r0
	std	-76(fp)			# zsq
	pushl	20(fp)			# hfs
	ldd	-76(fp)			# zsq
	pushd
	ldd	_p1			# p1
	pushd
	callf	$24,_Kmuld
	ldd	r0
	std	r2
	pushl	20(fp)			# hfs
	ldd 	_p0			# p0
	pushd
	ldd	r2
	pushd
	callf	$24,_Kaddd
	ldd	r0
	std	-84(fp)			# temp
	pushl	20(fp)			# hfs
	ldd	-76(fp)			# zsq
	pushd
	ldd	_q1
	pushd
	callf	$24,_Kmuld
	ldd	r0
	std	r2
	pushl	20(fp)			# hfs
	ldd	_q0			# q0
	pushd
	ldd	r2
	pushd
	callf	$24,_Kaddd
	ldd	r0
	std	r2			# temp result of (q1*zsq+q0)
	pushl	20(fp)			# hfs
	ldd	r2
	pushd
	ldd	-84(fp)			# temp
	pushd
	callf	$24,_Kdivd
	ldd	r0
	std	-84(fp)			# put in temp
	pushl	20(fp)			# hfs
	ldd	-68(fp)			# z
	pushd
	ldd	-84(fp)
	pushd
	callf	$24,_Kmuld
	ldd	r0
	std	r2
	pushl	20(fp)			#hfs
	pushl	$0			# 0 pad
	pushl	-88(fp)			# exp
	pushl	$0			# dummy
	pushl	$0			# dummy
	callf	$24,_Kcvtld
	pushl	20(fp)			# hfs
	ldd	_log2			# log2
	pushd
	pushl	r1			# exp converted
	pushl	r0			
	callf	$24,_Kmuld
	ldd	r0
	std	r4
	pushl	20(fp)			# hfs
	ldd	r2			# result of temp*z
	pushd
	ldd	r4
	pushd
	callf	$24,_Kaddd
	ldd	r0
	cvdf
	stf	r0
	clrl	r1
	ret

	.data
	.align	2
_log2:	.long	0x40317217, 0xf7d1cf7a # .double 0.69314718055994531
_ln10:	.long	0x41135d8d, 0xddaaa8ac # .double 2.3025850929940457
_sqrto2:.long	0x403504f3, 0x33f9de65 # .double 0.70710678118654753
_p0:	.long	0xc154114d, 0xeb0ba468 # .double -3.31355617479
_p1:	.long	0x40654226, 0x56bd0c4c # .double 0.89554061525
_q0:	.long	0xc0d4114c, 0xfdc7df02 # .double -1.65677797691
_q1:	.long	0x40800000, 0x00000000 # .double 1
small:	.long	0xfffffffe, 0xfffffffe # .double -1.7014117331926443e+38
half:	.long	0x40000000, 0x00000000 # .double 0.5
two:	.long	0x41000000, 0x00000000 # .double 2
one:	.long	0x40800000, 0x00000000 # .double 1
