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
 *
 * All recipients should regard themselves as participants in an ongoing
 * research project and hence should feel obligated to report their
 * experiences (good or bad) with these elementary function codes, using
 * the sendbug(8) program, to the authors.
 *
 *	@(#)support.s	5.3 (Berkeley) 6/30/88
 */
	.data
	.align	2
_sccsid:
.asciz	"@(#)support.s	1.3 (Berkeley) 8/21/85; 5.3 (ucb.elefunt) 6/30/88"

/*
 * copysign(x,y),
 * logb(x),
 * scalb(x,N),
 * finite(x),
 * drem(x,y),
 * Coded in vax assembly language by K.C. Ng,  3/14/85.
 * Revised by K.C. Ng on 4/9/85.
 */

/*
 * double copysign(x,y)
 * double x,y;
 */
	.globl	_copysign
	.text
	.align	1
_copysign:
	.word	0x4
	movq	4(ap),r0		# load x into r0
	bicw3	$0x807f,r0,r2		# mask off the exponent of x
	beql	Lz			# if zero or reserved op then return x
	bicw3	$0x7fff,12(ap),r2	# copy the sign bit of y into r2
	bicw2	$0x8000,r0		# replace x by |x|
	bisw2	r2,r0			# copy the sign bit of y to x
Lz:	ret

/*
 * double logb(x)
 * double x;
 */
	.globl	_logb
	.text
	.align	1
_logb:
	.word	0x0
	bicl3	$0xffff807f,4(ap),r0	# mask off the exponent of x
	beql    Ln
	ashl	$-7,r0,r0		# get the bias exponent
	subl2	$129,r0			# get the unbias exponent
	cvtld	r0,r0			# return the answer in double
	ret
Ln:	movq	4(ap),r0		# r0:1 = x (zero or reserved op)
	bneq	1f			# simply return if reserved op
	movq 	$0x0000fe00ffffcfff,r0  # -2147483647.0
1:	ret

/*
 * long finite(x)
 * double x;
 */
	.globl	_finite
	.text
	.align	1
_finite:
	.word	0x0000
	bicw3	$0x7f,4(ap),r0		# mask off the mantissa
	cmpw	r0,$0x8000		# to see if x is the reserved op
	beql	1f			# if so, return FALSE (0)
	movl	$1,r0			# else return TRUE (1)
	ret
1:	clrl	r0
	ret

/*
 * double scalb(x,N)
 * double x; int N;
 */
	.globl	_scalb
	.set	ERANGE,34
	.text
	.align	1
_scalb:
	.word	0xc
	movq	4(ap),r0
	bicl3	$0xffff807f,r0,r3
	beql	ret1			# 0 or reserved operand
	movl	12(ap),r2
	cmpl	r2,$0x12c
	bgeq	ovfl
	cmpl	r2,$-0x12c
	bleq	unfl
	ashl	$7,r2,r2
	addl2	r2,r3
	bleq	unfl
	cmpl	r3,$0x8000
	bgeq	ovfl
	addl2	r2,r0
	ret
ovfl:	pushl	$ERANGE
	calls	$1,_infnan		# if it returns
	bicw3	$0x7fff,4(ap),r2	# get the sign of input arg
	bisw2	r2,r0			# re-attach the sign to r0/1
	ret
unfl:	movq	$0,r0
ret1:	ret

/*
 * DREM(X,Y)
 * RETURN X REM Y =X-N*Y, N=[X/Y] ROUNDED (ROUNDED TO EVEN IN THE HALF WAY CASE)
 * DOUBLE PRECISION (VAX D format 56 bits)
 * CODED IN VAX ASSEMBLY LANGUAGE BY K.C. NG, 4/8/85.
 */
	.globl	_drem
	.set	EDOM,33
	.text
	.align	1
_drem:
	.word	0xffc
	subl2	$12,sp	
	movq	4(ap),r0		#r0=x
	movq	12(ap),r2		#r2=y
	jeql	Rop			#if y=0 then generate reserved op fault
	bicw3	$0x007f,r0,r4		#check if x is Rop
	cmpw	r4,$0x8000
	jeql	Ret			#if x is Rop then return Rop
	bicl3	$0x007f,r2,r4		#check if y is Rop
	cmpw	r4,$0x8000
	jeql	Ret			#if y is Rop then return Rop
	bicw2	$0x8000,r2		#y  := |y|
	movw	$0,-4(fp)		#-4(fp) = nx := 0
	cmpw	r2,$0x1c80		#yexp ? 57 
	bgtr	C1			#if yexp > 57 goto C1
	addw2	$0x1c80,r2		#scale up y by 2**57
	movw	$0x1c80,-4(fp)		#nx := 57 (exponent field)
C1:
	movw	-4(fp),-8(fp)		#-8(fp) = nf := nx
	bicw3	$0x7fff,r0,-12(fp)	#-12(fp) = sign of x
	bicw2	$0x8000,r0		#x  := |x|
	movq	r2,r10			#y1 := y
	bicl2	$0xffff07ff,r11		#clear the last 27 bits of y1
loop:
	cmpd	r0,r2			#x ? y
	bleq	E1			#if x <= y goto E1
 /* begin argument reduction */
	movq	r2,r4			#t =y
	movq	r10,r6			#t1=y1
	bicw3	$0x807f,r0,r8		#xexp= exponent of x
	bicw3	$0x807f,r2,r9		#yexp= exponent fo y
	subw2	r9,r8			#xexp-yexp
	subw2	$0x0c80,r8		#k=xexp-yexp-25(exponent bit field)
	blss	C2			#if k<0 goto C2
	addw2	r8,r4			#t +=k	
	addw2	r8,r6			#t1+=k, scale up t and t1
C2:
	divd3	r4,r0,r8		#x/t
	cvtdl	r8,r8			#n=[x/t] truncated
	cvtld	r8,r8			#float(n)
	subd2	r6,r4			#t:=t-t1
	muld2	r8,r4			#n*(t-t1)
	muld2	r8,r6			#n*t1
	subd2	r6,r0			#x-n*t1
	subd2	r4,r0			#(x-n*t1)-n*(t-t1)
	brb	loop
E1:
	movw	-4(fp),r6		#r6=nx
	beql	C3			#if nx=0 goto C3
	addw2	r6,r0			#x:=x*2**57 scale up x by nx
	movw	$0,-4(fp)		#clear nx
	brb	loop
C3:
	movq	r2,r4			#r4 = y
	subw2	$0x80,r4		#r4 = y/2
	cmpd	r0,r4			#x:y/2
	blss	E2			#if x < y/2 goto E2
	bgtr	C4			#if x > y/2 goto C4
	cvtdl	r8,r8			#ifix(float(n))
	blbc	r8,E2			#if the last bit is zero, goto E2
C4:
	subd2	r2,r0			#x-y
E2:
	xorw2	-12(fp),r0		#x^sign (exclusive or)
	movw	-8(fp),r6		#r6=nf
	bicw3	$0x807f,r0,r8		#r8=exponent of x
	bicw2	$0x7f80,r0		#clear the exponent of x
	subw2	r6,r8			#r8=xexp-nf
	bgtr	C5			#if xexp-nf is positive goto C5
	movw	$0,r8			#clear r8
	movq	$0,r0			#x underflow to zero
C5:
	bisw2	r8,r0			#put r8 into x's exponent field
	ret
Rop:					#Reserved operand
	pushl	$EDOM
	calls	$1,_infnan		#generate reserved op fault
	ret
Ret:
	movq	$0x8000,r0		#propagate reserved op
	ret
