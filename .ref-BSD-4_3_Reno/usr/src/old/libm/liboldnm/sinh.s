#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)sinh.s	5.1 (Berkeley) 5/8/85
# 
#
# double _sinh(arg)
# double _cosh(arg)
# double arg
# method: compute from exp except for sinh where -.5<arg<.5
# then a polynimial approx. is uded
# JF Jarvis, August 12,1978
.globl _sinh
.globl _cosh
.globl _exp
.text
.align 1
_sinh:
	.word	0x0c0
	bispsw	$0xe0
	movd	4(ap),r6
	cmpd	r6,$0d0.5e+0
	jgeq	expfrm
	cmpd	r6,$0d-0.5e+0
	jleq	expfrm
	muld3	r6,r6,r0	# Hart&Cheney SINH 1985
	polyd	r0,$5,pcoef
	muld2	r6,r0
	ret
expfrm:
	movd	r6,-(sp)	# sinh(x)=(exp(x)-exp(-x))/2
	calls	$2,_exp
	divd3	r0,$0d1.0e+0,r2
	subd2	r2,r0
	muld2	$0d0.5e+0,r0
	ret
.align 1
_cosh:
	.word	0x0
	bispsw	$0xe0
	movd	4(ap),-(sp)
	calls	$2,_exp
	divd3	r0,$0d1.0e+0,r2
	addd2	r2,r0
	muld2	$0d0.5e+0,r0
	ret
.data
.align	2
pcoef:
	.double 0d0.251726188251e-7
	.double 0d0.275569807356154e-5
	.double 0d0.1984127027907999e-3
	.double 0d0.833333333307759961e-2
	.double 0d0.16666666666667212324e+0
	.double 0d0.99999999999999998116e+0
