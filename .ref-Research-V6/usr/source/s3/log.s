.globl log, _log
ldfps = 170100^tst
stfps = 170200^tst
ldexp = 176400^movif
stexp = 175000^movfi
/
/	log accepts its argument and returns its result
/	in fr0.  The carry bit is set if the argument is
/	zero or negative.
/	The coefficients are #2705 from Hart & Cheney.
/
/	movf	arg,fr0
/	jsr	pc,log
/	movf	fr0,...
/
_log:
	mov	r5,-(sp)
	mov	sp,r5
	movf	4(r5),fr0
	jsr	pc,log
	mov	(sp)+,r5
	rts	pc

log:
	tstf	fr0
	cfcc
	bgt	1f
	movf	$bigneg,fr0	/return -(big) on error
	sec
	rts	pc
1:
	stfps	-(sp)
	ldfps	$200		/di mode
	movf	fr2,-(sp)
	movf	fr1,-(sp)
/
	stexp	fr0,-(sp)	/scale
	ldexp	$0,fr0
	cmpf	sqrt2o2,fr0
	cfcc
	blt	1f
	ldexp	$1,fr0
	dec	(sp)
1:
	movf	fr0,fr1		/(1/2)^(1/2) < x < 2^(1/2)
	subf	$one,fr0
	addf	$one,fr1
	divf	fr1,fr0		/z = (x-1)/(x+1)
	movf	fr0,fr1
	mulf	fr0,fr1		/z^2
/
	movf	p3,fr2
	mulf	fr1,fr2
	addf	p2,fr2
	mulf	fr1,fr2
	addf	p1,fr2
	mulf	fr1,fr2
	addf	p0,fr2
	mulf	fr2,fr0		/zP(z)
/
	movf	fr1,fr2
	addf	q2,fr2
	mulf	fr1,fr2
	addf	q1,fr2
	mulf	fr1,fr2
	addf	q0,fr2		/Q(z)
/
	divf	fr2,fr0		/zP(z)/Q(z)
	movif	(sp)+,fr1
	mulf	log2,fr1
	addf	fr1,fr0
/
	movf	(sp)+,fr1
	movf	(sp)+,fr2
	ldfps	(sp)+
	rts	pc
/
/
one	= 40200
bigneg	= 177777
/
.data
sqrt2o2: 40065; 02363; 31771; 157145
log2:	 40061; 71027;173721;147572
/
p0:	141300; 16201; 02154; 10216
p1:	 41367;124211; 21611;114442
p2:	141032; 31773; 64222; 40261
p3:	 37727;114303;110107;114145
/
q0:	141100; 16201; 02154; 10216
q1:	 41233;154404;136454; 22153
q2:	141016;111747; 07541; 52530
/
/
/p0 = -.24013 91795 59210 50986 8484  d2
/p1 =  .30957 29282 15376 50062 264   d2
/p2 = -.96376 90933 68686 59324       d1
/p3 =  .42108 73712 17979 7145        d0
/
/q0 = -.12006 95897 79605 25471 7525  d2
/q1 =  .19480 96607 00889 73051 623   d2
/q2 = -.89111 09027 93783 12337       d1
/q3 =  .1                             d1
/
