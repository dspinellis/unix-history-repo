#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)badtan.s	5.1 (Berkeley) %G%
# 
#
# double tan(arg)
# double arg;
#method: range reduction to [0,pi/4] followed by polynomial
# approximation:  Hart&Cheney TAN 4285 D=19.74
.globl	_tan
.text
.align	1
_tan:
	.word	0x07c0
	bispsw	$0xe0
	clrl	r10	# sign flag, !=0 negate result
	movd	4(ap),r0
	jgeq	c1
	xorw2	$0x8000,r10
	mnegd	r0,r0
c1:
	emodd	fopi,fopix,r0,r2,r6
	bicl2	$~3,r2
	caseb	r2,$0,$3
a0:	.word	a1-a0,a2-a0,a3-a0,a4-a0
a2:	subd3	r6,$0d1.0e+0,r6
	jbr	b1
a3: xorw2	$0x8000,r10
b1:	bsbb	rtan
	jneq	b2	# tests final result cmptd in rtan
	movd	$0d1.7e+38,r0
	jmp	b3
b2:	divd3	r0,$0d1.0e+0,r0
b3:	xorw2	r10,r0	# negate result if flag != 0 
	ret
a4:	subd3	r6,$0d1.0e+0,r6
	xorw2	$0x8000,r10
a1:	bsbb	rtan
	xorw2	r10,r0
	ret
rtan:
	muld3	r6,r6,r8
	polyd	r8,$4,pcoef
	muld2	r0,r6
	polyd	r8,$3,qcoef
	divd3	r0,r6,r0
	rsb
.data
.align	2
pcoef:
	.double 0d0.33866386426771720960e-4
	.double 0d0.34225543872410034353e-1
	.double 0d-0.15506856534832663769e+2
	.double 0d0.10559709017149531936e+4
	.double 0d-0.13068202647548256682e+5
qcoef:
	.double 0d1.0e+0
	.double 0d-0.15550331640317099669e+3
	.double 0d0.47657513629164836989e+4
	.double 0d-0.16638952389471190018e+5
fopi:
	.double 0d1.27323954473516268e+0
fopix: .byte 0x29
