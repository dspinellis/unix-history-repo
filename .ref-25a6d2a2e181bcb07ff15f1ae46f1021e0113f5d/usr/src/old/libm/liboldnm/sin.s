#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)sin.s	5.1 (Berkeley) %G%
# 
#
# double float sin(),cos()
#coefficients are from Hart & Cheney  SIN3370 (18.80D)
.text
.align 1
.globl _sin
.globl _cos
_sin:
	.word 0x03c0
	bispsw	$0xe0
	clrl	r9	
	movd	4(ap),r0
	jgeq	range
	movl	$2,r9
	jbr	negarg

.align 1
_cos:
	.word	0x0fc0
	bispsw	$0xe0
	movl	$1,r9
	movd	4(ap),r0
	jgeq	range
negarg:
	mnegd	r0,r0

range:
	emodd	twoopi,twoopix,r0,r8,r6
	addl2	r9,r8
	bicb2	$~03,r8
	caseb	r8,$0,$3
bse:	.word	qda-bse,qdb-bse,qdc-bse,qdd-bse
qdb:	subd3	r6,$0d1.0e+0,r6
	jbr	qda
qdc:	mnegd	r6,r6
	jbr	qda
qdd:	subd2	$0d1.0e+0,r6
qda:
	muld3	r6,r6,r8
	polyd	r8,$4,pcoef
	muld2	r0,r6
	polyd	r8,$4,qcoef
	divd3	r0,r6,r0
	ret
.data
.align 2
twoopi:.double	0d6.36619772367581340000e-01
qcoef:.double	0d1.00000000000000000000e+00
 .double	0d1.32653490878613630000e+02
 .double	0d9.46309610153820810000e+03
 .double	0d4.08179225234329970000e+05
 .double	0d8.64455865292253430000e+06
pcoef:.double	0d1.45968840666576870000e+02
 .double	0d-1.38472724998245280000e+04
 .double	0d4.40103053537526640000e+05
 .double	0d-4.94290810090284410000e+06
 .double	0d1.35788409787737560000e+07
twoopix: .byte 0x29
