#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)sqrt.s	5.1 (Berkeley) 5/8/85
# 
#
# double sqrt(arg):revised July 18,1980
# double arg
# if(arg<0.0) { _errno=EDOM; return(-sqrt(-arg)) }
# W. Kahan's magic sqrt
# coded by Heidi Stettner

	.set	EDOM,98
	.text
	.align	1
	.globl	_sqrt
	.globl	dsqrt_r5
	.globl	_errno
_sqrt:
	.word	0x003c          # save r5,r4,r3,r2
	bispsw	$0xe0
	movd	4(ap),r0
	bsbb	dsqrt_r5
	ret
dsqrt_r5:
	movd	r0,r4
	jleq	nonpos		#argument is not positive
	movzwl	r4,r2
	ashl	$-1,r2,r0
	addw2	$0x203c,r0	#r0 has magic initial appx

# Do two steps of Heron's rule

	divf3	r0,r4,r2	
	addf2	r2,r0
	subw2	$0x80,r0

	divf3	r0,r4,r2
	addf2	r2,r0
	subw2	$0x80,r0


# Scale argument and approximation to prevent over/underflow
# NOTE: The following four steps would not be necessary if underflow
#       were gentle.

	bicw3	$0xffff807f,r4,r1
	subw2	$0x4080,r1		# r1 contains scaling factor
	subw2	r1,r4
	movl	r0,r2
	subw2	r1,r2

# Cubic step

	clrl	r1
	clrl	r3
	muld2	r0,r2
	subd2	r2,r4
	addw2	$0x100,r2
	addd2	r4,r2
	muld2	r0,r4
	divd2	r2,r4
	addw2	$0x80,r4
	addd2	r4,r0
	rsb
nonpos:
	jneq	negarg
	clrd	r0		#argument is zero
	rsb
negarg:
	movl	$EDOM,_errno
	mnegd	r4,-(sp)
	calls	$2,_sqrt
	mnegd	r0,r0		# returns -sqrt(-arg)
	ret
