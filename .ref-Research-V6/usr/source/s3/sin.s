ldfps = 170100^tst
stfps = 170200^tst
/
.globl sin, _sin
.globl cos, _cos
/
/	floating point sin/cos
/	replaces the value in fr0 by its sin/cos
/	there are no error exits
/	coefficients are #3370 from Hart & Cheney
/
_sin:
	mov	r5,-(sp)
	mov	sp,r5
	movf	4(r5),fr0
	jsr	pc,sin
	br	1f

_cos:
	mov	r5,-(sp)
	mov	sp,r5
	movf	4(r5),fr0
	jsr	pc,cos
1:
	mov	(sp)+,r5
	rts	pc

cos:
	absf	fr0
	mov	$1,-(sp)
	br	1f
sin:
	clr	-(sp)
1:
	stfps	-(sp)
	ldfps	$200
	movf	fr1,-(sp)
	movf	fr2,-(sp)
	mov	r0,-(sp)
/
/	quadrant reduction -  arg = (2/J)x
/	-1 < arg < 1
/
	movf	fr0,-(sp)
	absf	fr0
	modf	frpi2,fr0
	modf	$fourth,fr1
	mulf	$four,fr1
	movfi	fr1,r0
	add	34(sp),r0
	movf	$one,fr1
	inc	r0
	ror	r0
	bcs	1f
	subf	$one,fr0
1:
	ror	r0
	bcc	1f
	negf	fr0
1:
	tstf	(sp)+
	cfcc
	bpl	1f
	negf	fr0
1:
	movf	fr0,fr1
	mulf	fr0,fr1		/arg^2
/
	movf	p4,fr2
	mulf	fr1,fr2
	addf	p3,fr2
	mulf	fr1,fr2
	addf	p2,fr2
	mulf	fr1,fr2
	addf	p1,fr2
	mulf	fr1,fr2
	addf	p0,fr2
	mulf	fr2,fr0		/ zP(z^2)
/
	movf	fr1,fr2
	addf	q3,fr2
	mulf	fr1,fr2
	addf	q2,fr2
	mulf	fr1,fr2
	addf	q1,fr2
	mulf	fr1,fr2
	addf	q0,fr2		/ Q(z^2)
/
	divf	fr2,fr0		/ zP(z^2)/Q(z^2)
/
	mov	(sp)+,r0
	movf	(sp)+,fr2
	movf	(sp)+,fr1
	ldfps	(sp)+
	tst	(sp)+
/	clc		/tst clears carry
	rts	pc
/
fourth	= 37600
one	= 40200
four	= 40600
/
.data
frpi2:	40042;174603; 67116; 42025
/
p0:	046117;031130;175220;165273
p1:	145626;154170;031651;104637
p2:	044726;162341;133224;052302
p3:	143530;056427;005061;125021
p4:	042021;174005;170441;175607
q0:	046003;163716;123445;167144
q1:	044707;047147;032436;120046
q2:	043423;156142;064161;007314
q3:	042004;123513;026637;160477
/
/p0 =  .13578 84097 87737 56690 92680 d8
/p1 = -.49429 08100 90284 41611 58627 d7
/p2 =  .44010 30535 37526 65019 44918 d6
/p3 = -.13847 27249 98245 28730 54457 d5
/p4 =  .14596 88406 66576 87222 26959 d3
/q0 =  .86445 58652 92253 44299 15149 d7
/q1 =  .40817 92252 34329 97493 95779 d6
/q2 =  .94630 96101 53820 81805 71257 d4
/q3 =  .13265 34908 78613 63589 11494 d3
/q4 =  .1                             d1
