.globl gamma, _gamma, signgam, _signgam
.globl	log, sin
half = 040000
one = 40200
two = 40400
eight = 41000
large = 77777
ldfps = 170100^tst
stfps = 170200^tst
/
/	gamma computes the log of the abs of the gamma function.
/	gamma accepts its argument and returns its result
/	in fr0.  The carry bit is set if the result is
/	too large to represent.
/	The sign of the gamma function is
/	returned in the globl cell signgam.
/
/	The coefficients for expansion around zero
/	are #5243 from Hart & Cheney; for expansion
/	around infinity they are #5404.
/
/	movf	arg,fr0
/	jsr	pc,gamma
/	movf	fr0,...
/

_gamma:
	mov	r5,-(sp)
	mov	sp,r5
	movf	4(r5),fr0
	jsr	pc,gamma
	mov	(sp)+,r5
	rts	pc
gamma:
	stfps	-(sp)
	ldfps	$200
	clr	signgam
	movf	fr1,-(sp)
	tstf	fr0
	cfcc
	ble	negative
	cmpf	$eight,fr0
	cfcc
	blt	asymptotic
	jsr	pc,regular
/
lret:
	jsr	pc,log
	bec	ret
	4
ret:
	movf	(sp)+,fr1
	ldfps	(sp)+
	clc
	rts	pc
/
erret:
	movf	$large,fr0
	movf	(sp)+,fr1
	ldfps	(sp)+
	sec
	rts	pc

/
/	here for positive x > 8
/	the log of the gamma function is
/	approximated directly by the asymptotic series.
/
asymptotic:
	movf	fr0,-(sp)
	movf	fr0,fr1
	jsr	pc,log
	subf	$half,fr1
	mulf	fr1,fr0
	subf	(sp),fr0
	addf	goobie,fr0
/
	movf	$one,fr1
	divf	(sp)+,fr1
	movf	fr0,-(sp)
	movf	fr1,-(sp)
	mulf	fr1,fr1
/
	mov	r0,-(sp)
	mov	$p5p,r0
	mov	$5,-(sp)
	movf	*(r0)+,fr0
1:
	mulf	fr1,fr0
	addf	*(r0)+,fr0
	dec	(sp)
	bne	1b
	tst	(sp)+
	mov	(sp)+,r0
	mulf	(sp)+,fr0
	addf	(sp)+,fr0
	br	ret

/
/	here on negative
/	the negative gamma is computed
/	in terms of the pos gamma.
/
negative:
	absf	fr0
	movf	fr0,fr1
	mulf	pi,fr0
	jsr	pc,sin
	negf	fr0
	cfcc
	beq	erret
	bgt	1f
	inc	signgam
	absf	fr0
1:
	mov	signgam,-(sp)
	mulf	fr1,fr0
	divf	pi,fr0
	jsr	pc,log
	movf	fr0,-(sp)
	movf	fr1,fr0
	jsr	pc,gamma
	addf	(sp)+,fr0
	negf	fr0
	mov	(sp)+,signgam
	br	ret

/
/	control comes here for arguments less than 8.
/	if the argument is 2<x<3 then compute by
/	a rational approximation.
/	if x<2 or x>3 then the argument
/	is reduced in range by the formula
/	gamma(x+1) = x*gamma(x)
/
regular:
	subf	$two,fr0
	cfcc
	bge	1f
	addf	$two,fr0
	movf	fr0,-(sp)
	addf	$one,fr0
	movf	fr0,-(sp)
	addf	$one,fr0
	jsr	pc,regular
	divf	(sp)+,fr0
	divf	(sp)+,fr0
	rts	pc
1:
	cmpf	$one,fr0
	cfcc
	bgt	1f
	addf	$one,fr0
	movf	fr0,-(sp)
	subf	$two,fr0
	jsr	pc,1b
	mulf	(sp)+,fr0
	rts	pc
1:
	movf	fr2,-(sp)
	mov	r0,-(sp)
	mov	$p4p,r0
	mov	$6,-(sp)
	movf	fr0,fr2
	movf	*(r0)+,fr0
1:
	mulf	fr2,fr0
	addf	*(r0)+,fr0
	dec	(sp)
	bne	1b
	mov	$7,(sp)
	movf	fr2,fr1
	br	2f
1:
	mulf	fr2,fr1
2:
	addf	*(r0)+,fr1
	dec	(sp)
	bne	1b
	tst	(sp)+
	mov	(sp)+,r0
	divf	fr1,fr0
	movf	(sp)+,fr2
	rts	pc
/
.data
p4p:
	p6;p5;p4;p3;p2;p1;p0
	q6;q5;q4;q3;q2;q1;q0

/	p6 = -.67449 50724 59252 89918 d1
/	p5 = -.50108 69375 29709 53015 d2
/	p4 = -.43933 04440 60025 67613 d3
/	p3 = -.20085 27401 30727 91214 d4
/	p2 = -.87627 10297 85214 89560 d4
/	p1 = -.20886 86178 92698 87364 d5
/	p0 = -.42353 68950 97440 89647 d5
/	q7 = 1.0 d0
/	q6 = -.23081 55152 45801 24562 d2
/	q5 = +.18949 82341 57028 01641 d3
/	q4 = -.49902 85266 21439 04834 d3
/	q3 = -.15286 07273 77952 20248 d4
/	q2 = +.99403 07415 08277 09015 d4
/	q1 = -.29803 85330 92566 49932 d4
/	q0 = -.42353 68950 97440 90010 d5
p1:
	143643
	26671
	36161
	72154
p2:
	143410
	165327
	54121
	172630
p3:
	142773
	10340
	74264
	152066
p4:
	142333
	125113
	176657
	75740
p5:
	141510
	67515
	65111
	24263
p6:
	140727
	153242
	163350
	32217
p0:
	144045
	70660
	101665
	164444
q1:
	143072
	43052
	50302
	136745
q2:
	43433
	50472
	145404
	175462
q3:
	142677
	11556
	144553
	154177
q4:
	142371
	101646
	141245
	11264
q5:
	42075
	77614
	43022
	27573
q6:
	141270
	123404
	76130
	12650
q0:
	144045
	70660
	101665
	164444

p5p:
	s5;s4;s3;s2;s1;s0
/
/	s5 = -.16334 36431 d-2
/	s4 = +.83645 87892 2 d-3
/	s3 = -.59518 96861 197 d-3
/	s2 = +.79365 05764 93454 d-3
/	s1 = -.27777 77777 35865 004 d-2
/	s0 = +.83333 33333 33331 01837 d-1
/	goobie = 0.91893 85332 04672 74178 d0
s5:
	135726
	14410
	15074
	17706
s4:
	35533
	42714
	111634
	76770
s3:
	135434
	3200
	171173
	156141
s2:
	35520
	6375
	12373
	111437
s1:
	136066
	5540
	132625
	63540
s0:
	37252
	125252
	125252
	125047
goobie:
	40153
	37616
	41445
	172645
pi:
	40511
	7732
	121041
	64302
.bss
_signgam:
signgam:.=.+2
