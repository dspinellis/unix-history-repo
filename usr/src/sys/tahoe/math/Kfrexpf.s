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
 *	@(#)Kfrexpf.s	7.1 (Berkeley) 12/6/90
 */

#include "../tahoe/SYS.h"

	.text
ENTRY(Kfrexpf, 0)
	subl3	$60,fp,sp
	clrl	-60(fp)		# j=0;
	clrl	-56(fp)		# neg=0;
	tstl	4(fp)		# if(x<0){
	jgeq	1f
	lnd	4(fp)
	std	4(fp)		# x = -x;
	movl	$1,-56(fp)	# neg=1;}
1:
	cmpd2	4(fp),one	# if (x>1){
	jleq	1f
2:
	cmpd2	4(fp),one	# while(x>1){
	jleq	3f
	addl2	$1,-60(fp)	# j=j+1;
	pushl	16(fp)		# hfs	
	ldd	two
	pushd
	ldd	4(fp)
	pushd
	callf	$24,_Kdivd
	ldd	r0
	std	4(fp)		# x= x/2;
	jbr	2b
1:
	cmpd2	4(fp),half	# if(x<0.5){
	jlss	2f
	jbr	3f
0:
	subl2	$1,-60(fp)	# j = j-1;
	pushl	16(fp)		# hfs
	ldd	4(fp)
	pushd
	ldd	two
	pushd
	callf	$24,_Kmuld
	ldd	r0
	std	4(fp)		# x = 2*x;
2:
	cmpd2	4(fp),half	# while (x<0.5){
	jlss	0b
3:
	movl	-60(fp),*12(fp)	# *i=j;
	tstl	-56(fp)		# if (neg)
	jeql	1f
	lnd	4(fp)
	std	4(fp)
1:
	ldd	4(fp)
	cvdf
	stf	r0
	ret

	.data
	.align	2
one:	.long	0x40800000, 0x00000000 # .double 1
two:	.long	0x41000000, 0x00000000 # .double 2
half:	.long	0x40000000, 0x00000000 # .double .5
