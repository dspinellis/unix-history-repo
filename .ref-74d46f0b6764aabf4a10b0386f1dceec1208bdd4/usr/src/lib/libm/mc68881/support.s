/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)support.s	5.2 (Berkeley) %G%
 */

	.text
	.globl	_copysign, _finite, _scalb, _logb, _drem, _pow_p, _atan2__A

| copysign(x,y) 
| returns x with the sign of y. 
_copysign:
	movl	sp@(4),d0
	movl	sp@(8),d1
	tstw	sp@(12)
	jmi	Lneg
	bclr	#31,d0
	rts
Lneg:
	bset	#31,d0
	rts

| finite(x)
| returns the value TRUE if -INF < x < +INF and returns FALSE otherwise.
_finite:
	movw	#0x7FF0,d0
	movw	sp@(4),d1
	andw	d0,d1
	cmpw	d0,d1
	beq	Lnotfin
	moveq	#1,d0
	rts
Lnotfin:
	clrl	d0
	rts

| scalb(x, N)
| returns  x * (2**N), for integer values N.
_scalb:
	fmoved	sp@(4),fp0
	fbeq	Ldone
	fscalel	sp@(12),fp0
Ldone:
	fmoved	fp0,sp@-
	movel	sp@+,d0
	movel	sp@+,d1
	rts

| logb(x)
| returns the unbiased exponent of x, a signed integer in double precision,
| except that logb(0) is -INF, logb(INF) is +INF, and logb(NAN) is that NAN.
_logb:
	movw	sp@(4),d0
	movw	#0x7FF0,d1	| exponent bits
	andw	d1,d0		| mask off all else
	cmpw	d1,d0		| max exponent?
	bne	Lfinite		| no, is finite
	fmoved	sp@(4),fp0	| yes, infinite or NaN
	fbun	Ldone		| NaN returns NaN
	fabsx	fp0		| +-inf returns inf
	jra	Ldone
Lfinite:
	fmoved	sp@(4),fp0	| get entire number
	fbne	Lnonz		| zero?
	flog2x	fp0		| yes, log(0) a convenient source of -inf
	jra	Ldone
Lnonz:
	fgetexpx	fp0	| get exponent
	jra	Ldone

| drem(x,y)
| returns  x REM y  =  x - [x/y]*y , where [x/y] is the integer nearest x/y;
| in half way case, choose the even one.
_drem:
	fmoved	sp@(4),fp0
	fremd	sp@(12),fp0
	fmoved	fp0,sp@-
	movel	sp@+,d0
	movel	sp@+,d1
	rts

| pow_p(x,y)
| return x**y for x with sign=1 and finite y
_pow_p:
	flognd	sp@(4),fp0
	fmuld	sp@(12),fp0
	fetoxx	fp0
	fmoved	fp0,sp@-
	movel	sp@+,d0
	movel	sp@+,d1
	rts

| atan2__A(y,x)
| compute atan2(y,x) where x,y are finite and non-zero
| called by atan2() after weeding out all the special cases
_atan2__A:
	moveq	#0,d0		| sign of result
	fmoved	sp@(4),fp0	| get y
	fboge	Lypos		| <0?
	moveq	#1,d0		| yes, result is neg
	fnegx	fp0		| make y pos
Lypos:
	fmoved	sp@(12),fp1	| get x
	fboge	Lxpos		| <0?
	fnegx	fp1		| yes, make x pos
	fdivx	fp1,fp0		| y/x
	fatanx	fp0,fp1		| atan(y/x)
	fmovecr	#0,fp0		| get pi
	fsubx	fp1,fp0		| pi - atan(y/x)
	jra	Lsetsign
Lxpos:
	fdivx	fp1,fp0		| y/x
	fatanx	fp0		| atan(y/x)
Lsetsign:
	tstl	d0		| should be neg?
	jeq	Lrpos		| no, all done
	fnegx	fp0		| yes, negate
Lrpos:
	fmoved	fp0,sp@-
	movel	sp@+,d0
	movel	sp@+,d1
	rts
