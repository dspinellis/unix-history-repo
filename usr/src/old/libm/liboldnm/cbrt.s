#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)cbrt.s	5.1 (Berkeley) %G%
# 
#
# double cbrt(arg)
# double arg
# no error exits
#method: range reduction to [1/8,1], poly appox, newtons method
# J F Jarvis, August 10,1978
.globl	_cbrt
.text
.align	1
_cbrt:
	.word	0x00c0
	bispsw	$0xe0
	clrl	r3
	movd	4(ap),r4
	jgtr	range
	jeql	retz
	mnegd	r4,r4	# |arg| in r0,r1
	movl	$0x100,r3	# sign bit of result
range:
	extzv	$7,$8,r4,r6
	insv	$128,$7,$8,r4	# 0.5<= frac: r4,r5 <1.0
	clrl	r7
	ediv	$3,r6,r6,r7	# r6= expnt/3; r7= expnt%3
	addb2	$86,r6
	bisl2	r3,r6	# sign,exponent of result
	polyf	r4,$3,pcoef	# initial estimate is Hart&Cheney CBRT 0642
						# D=4.1
	muld3	r0,r0,r2	# Newtons method, iteration 1, H&C 6.1.10
	divd3	r2,r4,r2
	subd3	r2,r0,r2
	muld2	third,r2
	subd2	r2,r0	# D=8.2
	muld3	r0,r0,r2	# iteration 2
	divd3	r2,r4,r2
	subd3	r2,r0,r2
	muld2	third,r2
	subd2	r2,r0
	muld2	hc[r7],r0	# set range
	insv	r6,$7,$9,r0	# set sign,exponent
	ret
retz:
	clrd	r0
	ret
.data
.align	2
third: .double 0d0.33333333333333333333e+0
hc:
	.double 0d1.25992104989487316476e+0
	.double 0d1.58740105196819947475e+0
	.double	0d1.0e+0
pcoef:
	.float 0f0.1467073818e+0
	.float 0f-0.5173964673e+0
	.float 0f0.9319858515e+0
	.float 0f0.4387762363e+0
