# Copyright (c) 1985 Regents of the University of California.
# All rights reserved.
#
# %sccs.include.redist.sh%
#
#	@(#)sqrt.s	5.4 (Berkeley) %G%
#
	.data
	.align	2
_sccsid:
.asciz	"@(#)sqrt.s	1.1 (Berkeley) 8/21/85; 5.4 (ucb.elefunt) %G%"

/*
 * double sqrt(arg)   revised August 15,1982
 * double arg;
 * if(arg<0.0) { _errno = EDOM; return(<a reserved operand>); }
 * if arg is a reserved operand it is returned as it is
 * W. Kahan's magic square root
 * coded by Heidi Stettner and revised by Emile LeBlanc 8/18/82
 *
 * entry points:_d_sqrt		address of double arg is on the stack
 *		_sqrt		double arg is on the stack
 */
	.text
	.align	1
	.globl	_sqrt
	.globl	_d_sqrt
	.globl	libm$dsqrt_r5
	.set	EDOM,33

_d_sqrt:
	.word	0x003c          # save r5,r4,r3,r2
	movq	*4(ap),r0
	jmp  	dsqrt2
_sqrt:
	.word	0x003c          # save r5,r4,r3,r2
	movq    4(ap),r0
dsqrt2:	bicw3	$0x807f,r0,r2	# check exponent of input
	jeql	noexp		# biased exponent is zero -> 0.0 or reserved
	bsbb	libm$dsqrt_r5
noexp:	ret

/* **************************** internal procedure */

libm$dsqrt_r5:			# ENTRY POINT FOR cdabs and cdsqrt
				# returns double square root scaled by
				# 2^r6

	movd	r0,r4
	jleq	nonpos		# argument is not positive
	movzwl	r4,r2
	ashl	$-1,r2,r0
	addw2	$0x203c,r0	# r0 has magic initial approximation
/*
 * Do two steps of Heron's rule
 * ((arg/guess) + guess) / 2 = better guess
 */
	divf3	r0,r4,r2
	addf2	r2,r0
	subw2	$0x80,r0	# divide by two

	divf3	r0,r4,r2
	addf2	r2,r0
	subw2	$0x80,r0	# divide by two

/* Scale argument and approximation to prevent over/underflow */

	bicw3	$0x807f,r4,r1
	subw2	$0x4080,r1		# r1 contains scaling factor
	subw2	r1,r4
	movl	r0,r2
	subw2	r1,r2

/* Cubic step
 *
 * b = a + 2*a*(n-a*a)/(n+3*a*a) where b is better approximation,
 * a is approximation, and n is the original argument.
 * (let s be scale factor in the following comments)
 */
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
	pushl	$EDOM
	calls	$1,_infnan	# generate the reserved op fault
	ret
