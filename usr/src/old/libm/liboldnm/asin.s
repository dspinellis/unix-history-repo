#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)asin.s	5.1 (Berkeley) 5/8/85
# 
#
#double asin(arg)
#double arg; -1.0<=arg<=1.0 or an out of range error return
#method: call atan() after range reduction
# calls: satan (internal entry point of atan.s), _sqrt
# J F Jarvis August 8, 1078
.globl	_asin
.globl	_errno
.globl	_sqrt
.globl	satan
.set	EDOM,33
.text
.align	1
_asin:
	.word	0x3c0
	bispsw	$0xe0
	movd	4(ap),r6
	jgtr	a1
	mnegd	r6,r6
a1:	cmpd	r6,$0d1.0e+0
	jleq	a2
	movl	$EDOM,_errno
	clrd	r0
	ret
#
a2:	muld3	r6,r6,r0
	subd3	r0,$0d1.0e+0,-(sp)
	calls	$2,_sqrt
	cmpd	r6,$0d0.6875
	jleq	a3
	divd2	r6,r0
	jsb	satan
	subd3	r0,pio2,r0
	jbr	a4
a3:	divd3	r0,r6,r0
	jsb	satan
a4:	tstd	4(ap)
	jleq	a5
	ret
a5:	mnegd	r0,r0
	ret
.data
.align	2
pio2: .double 0d1.57079632679489661923e+0
