From Prof. Kahan at UC at Berkeley
# 
# Copyright (c) 1985 Regents of the University of California.
# 
# Use and reproduction of this software are granted  in  accordance  with
# the terms and conditions specified in  the  Berkeley  Software  License
# Agreement (in particular, this entails acknowledgement of the programs'
# source, and inclusion of this notice) with the additional understanding
# that  all  recipients  should regard themselves as participants  in  an
# ongoing  research  project and hence should  feel  obligated  to report
# their  experiences (good or bad) with these elementary function  codes,
# using "sendbug 4bsd-bugs@BERKELEY", to the authors.
#

# @(#)support.s	1.2 (Berkeley) %G%

# copysign(x,y),
# logb(x),
# scalb(x,N),
# finite(x),
# drem(x,y),
# sqrt(x) for Vax D format.
# Coded in vax assembly language by K.C. Ng,  3/14/85.
# Revised by K.C. Ng on 4/9/85.

# double copysign(x,y)
# double x,y;
	.text
	.align	1
	.globl	_copysign
_copysign:
	.word	0x0004
	movq	4(ap),r0		# load x into r0
	beql	Lz			# if zero return zero
	bicw3	$0x7fff,12(ap),r2	# copy the sign bit of y into r2
	bicw2	$0x8000,r0		# replace x by |x|
	bisw2	r2,r0			# copy the sign bit of y to x
Lz:	ret

# double logb(x)
# double x;
	.text
	.align	1
	.globl	_logb
_logb:
	.word	0x0000
	bicl3	$0xffff807f,4(ap),r0	# mask off the exponent of x
	ashl	$-7,r0,r0		# get the bias exponent
	beql    Ln
	subl2	$129,r0			# get the unbias exponent
	cvtld	r0,r0			# return the answer in double
	ret
Ln:	movq 	$0x0000fe00ffffcfff,r0   # -2147483647.0
        ret


# long finite(x)
# double x;
	.text
	.align	1
	.globl	_finite
_finite:
	.word	0x0000
	movl	$1,r0
	ret


# double scalb(x,N) double x; int N;

	.set	ERANGE,34		# out of range flag
	.text
	.align	1
	.globl	_errno
	.globl	_scalb
_scalb:
	.word	0x0004
	movq	4(ap),r0
	bicl3	$0xffff807f,r0,r3
	beql	ret1		# 0 or reserved operand
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
ovfl:	movl	$ERANGE, _errno
	movq	$0x8000,r0	# return reserve operand
	ret
unfl:	movq	$0,r0
ret1:	ret

# double sqrtd(arg)   revised August 15,1982
# double arg
# if(arg<0.0) { _errno=EDOM; return(<a reserved operand>) }
# if arg is a reserved operand it is returned as it is
# W. Kahan's magic square root
# coded by Heidi Stettner and revised by Emile LeBlanc 8/18/82

# entry points:	_d_sqrt		address of double arg is on the stack
#		_sqrt		double arg is on the stack

	.set	EDOM,33		# out of domain flag
	.text
	.align	1
	.globl	_sqrt
	.globl	_d_sqrt
	.globl	dsqrt_r5
	.globl	_errno

_d_sqrt:
	.word	0x003c          # save r5,r4,r3,r2
	movq	*4(ap),r0
	jmp  	dsqrt2
_sqrt:
	.word	0x003c          # save r5,r4,r3,r2
	movq    4(ap),r0
dsqrt2:	bicw3	$0x807f,r0,r2	# check exponent of input
	jeql	noexp		# biased exponent is zero -> 0.0 or reserved
	bsbb	dsqrt_r5
noexp:	ret

# **************************** internal procedure

dsqrt_r5:			# ENTRY POINT FOR cdabs and cdsqrt
				# returns double square root scaled by
				# 2^r6

	movd	r0,r4
	jleq	nonpos		# argument is not positive
	movzwl	r4,r2
	ashl	$-1,r2,r0
	addw2	$0x203c,r0	# r0 has magic initial approximation

# Do two steps of Heron's rule
# ((arg/guess) + guess) / 2 = better guess

	divf3	r0,r4,r2
	addf2	r2,r0
	subw2	$0x80,r0	# divide by two

	divf3	r0,r4,r2
	addf2	r2,r0
	subw2	$0x80,r0	# divide by two


# Scale argument and approximation to prevent over/underflow

	bicw3	$0x807f,r4,r1
	subw2	$0x4080,r1		# r1 contains scaling factor
	subw2	r1,r4
	movl	r0,r2
	subw2	r1,r2

# Cubic step

# b = a + 2*a*(n-a*a)/(n+3*a*a) where b is better approximation,
# a is approximation, and n is the original argument.
# (let s be scale factor in the following comments)

	clrl	r1
	clrl	r3
	muld2	r0,r2			# r2:r3 = a*a/s
	subd2	r2,r4			# r4:r5 = n/s - a*a/s
	addw2	$0x100,r2		# r2:r3 = 4*a*a/s
	addd2	r4,r2			# r2:r3 = n/s + 3*a*a/s
	muld2	r0,r4			# r4:r5 = a*n/s - a*a*a/s
	divd2	r2,r4			# r4:r5 = a*(n-a*a)/(n+3*a*a)
	addw2	$0x80,r4		# r4:r5 = 2*a*(n-a*a)/(n+3*a*a)
	addd2	r4,r0			# r0:r1 = a + 2*a*(n-a*a)/(n+3*a*a)
	rsb				# DONE!

nonpos:
	jneq	negarg
	ret			# argument and root are zero
negarg:
	movl	$EDOM,_errno
	movq	$0x8000,r0	# return reserve operand
	ret


# DREM(X,Y)
# RETURN X REM Y =X-N*Y, N=[X/Y] ROUNDED (ROUNDED TO EVEN IN THE HALF WAY CASE)
# DOUBLE PRECISION (VAX D format 56 bits)
# CODED IN VAX ASSEMBLY LANGUAGE BY K.C. NG, 4/8/85.
	.set	EDOM,33		# out of domain flag
	.text
	.align	1
	.globl	_drem
_drem:
	.word	0x0ffc
	subl2	$12,sp	
	movq	4(ap),r0		#r0=x
	movq	12(ap),r2		#r2=y
	jeql	Rop			#if y=0 return Reserved op (Rop)
	bicw3	$0x007f,r0,r4		#check if x is Rop
	cmpw	r4,$0x8000
	jeql	Rop			#if x is Rop goto Rop	
	bicl3	$0x007f,r2,r4		#check if y is Rop
	cmpw	r4,$0x8000
	jeql	Rop			#if y is Rop goto Rop	
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
#begin argument reduction
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
	subw2	$0x0080,r4		#r4 = y/2
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
	movl	$EDOM,_errno
	movq	$0x8000,r0		# return reserve operand
	ret

