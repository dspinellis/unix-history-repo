ldfps = 170100^tst
stfps = 170200^tst
/
.globl	atan, _atan
.globl	atan2, _atan2
/
/	floating-point arctangent
/
/	atan replaces the value in fr0 by its arctangent
/	in the range [-pi/2,pi/2].
/
/	atan2 places in fr0 the arctangent of fr0/fr1
/	in the range [-pi,pi].
/
/	there are no error exits
/
/	coefficients are #5076 from Hart & Cheney.
/
/

_atan:
	mov	r5,-(sp)
	mov	sp,r5
	movf	4(r5),fr0
	jsr	pc,atan
	br	1f

_atan2:
	mov	r5,-(sp)
	mov	sp,r5
	movf	4(r5),fr0
	movf	12.(r5),fr1
	jsr	pc,atan2
1:
	mov	(sp)+,r5
	rts	pc
atan:
	jsr	r0,save
	tstf	fr0
	cfcc
	blt	1f
	jsr	pc,satan
	br	ret
1:
	negf	fr0
	jsr	pc,satan
	negf	fr0
	br	ret
/
atan2:
	jsr	r0,save
	clr	-(sp)
	tstf	fr0
	cfcc
	bge	1f
	inc	(sp)
	negf	fr0
1:
	tstf	fr1
	cfcc
	beq	2f
	bgt	1f
	add	$2,(sp)
	negf	fr1
1:
	divf	fr1,fr0
	jsr	pc,satan
	br	1f
2:
	movf	pi2,fr0
1:
	bit	$2,(sp)
	beq	1f
	negf	fr0
	addf	pi2,fr0
	addf	pi2,fr0
1:
	bit	$1,(sp)+
	beq	1f
	negf	fr0
1:

ret:
	ldfps	(sp)+
	movf	(sp)+,fr3
	movf	(sp)+,fr2
	movf	(sp)+,fr1
	mov	(sp)+,r0
	rts	pc

save:
	movf	fr1,-(sp)
	movf	fr2,-(sp)
	movf	fr3,-(sp)
	stfps	-(sp)
	ldfps	$40200		/ DP, no interrupt
	jmp	(r0)

satan:
	cmpf	sq2m1,fr0
	cfcc
	bge	arctan
	cmpf	sq2p1,fr0
	cfcc
	bgt	1f
	movf	one,fr1
	divf	fr0,fr1
	movf	fr1,fr0
	jsr	pc,arctan
	negf	fr0
	addf	pi2,fr0
	rts	pc
1:
	movf	fr0,fr1
	subf	one,fr0
	addf	one,fr1
	divf	fr1,fr0
	jsr	pc,arctan
	addf	pi4,fr0
	rts	pc
arctan:
	mov	$p4p,r0
	mov	$4,-(sp)
	movf	fr0,fr3
	mulf	fr3,fr3
	movf	*(r0)+,fr1
1:
	mulf	fr3,fr1
	addf	*(r0)+,fr1
	dec	(sp)
	bne	1b
	mov	$4,(sp)
	movf	fr3,fr2
	br	2f
1:
	mulf	fr3,fr2
2:
	addf	*(r0)+,fr2
	dec	(sp)
	bne	1b
	tst	(sp)+
	divf	fr2,fr1
	mulf	fr1,fr0
	rts	pc

.data

p4p:
	p4;p3;p2;p1;p0
	q3;q2;q1;p0

one:	40200;0;0;0
pi2:	40311;7732;121041;64302
pi4:	40111;7732;121041;64302

sq2p1:	40432
	101171
	114774
	167461

sq2m1:	37724
	11714
	147747
	74621

p4:	37442
	145026
	75504
	15621
p3:	40725
	21566
	115517
	50305
p2:	41443
	160206
	172714
	25441
p1:	41632
	172223
	76027
	56645
p0:	41462
	25125
	6152
	126064
q3:	41170
	10112
	141724
	64324
q2:	41573
	53776
	25372
	71522
q1:	41670
	123114
	51576
	75020

/	one = 1.00000 00000 00000 00000 d0
/	sq2p1=2.41421 35623 73095 04880 d0
/	sq2m1= .41421 35623 73095 04880 d0
/	pi2 = 1.57079 63267 94896 61923  d0
/	p4 = .15897 40288 48230 7048 d0
/	p3 = .66605 79017 00926 2658 d1
/	p2 = .40969 26483 21022 5637 d2
/	p1 = .77477 68771 92042 0862 d2
/	p0 = .44541 34005 92906 8032 d2
/	q4 = .10000 00000 00000 0000 d1
/	q3 = .15503 97755 14219 8753 d2
/	q2 = .62835 93051 10323 7683 d2
/	q1 = .92324 80107 23009 7484 d2
/	q0 = .44541 34005 92906 8044 d2
