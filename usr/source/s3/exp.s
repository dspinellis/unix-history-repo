.globl exp, _exp
/
ldfps = 170100^tst
stfps = 170200^tst
ldexp = 176400^movif
stexp = 175000^movfi
/
/	exp accepts its argument and returns its result
/	in fr0.  The carry bit is set if the result overflows.
/	The coefficients are #1067 from Hart & Cheney.
/
/	movf	arg,fr0
/	jsr	pc,exp
/	movf	fr0,result
/
_exp:
	mov	r5,-(sp)
	mov	sp,r5
	mov	4(r5),fr0
	jsr	pc,exp
	mov	(sp)+,r5
	rts	pc

exp:
	stfps	-(sp)
	ldfps	$200		/di mode
	movf	fr2,-(sp)
	movf	fr1,-(sp)
	tstf	fr0
	cfcc
	bne	1f
	movf	$one,fr0	/exp(0)
	clc
	br	out
1:
	modf	log2e,fr0	/exp(x) = 2^(x*log2(e))
	cfcc
	bmi	2f
	movfi	fr1,-(sp)	/save integer part
	subf	$half,fr0
	br	3f
2:
	movfi	fr1,-(sp)
	dec	(sp)
	addf	$half,fr0
3:
	movf	fr0,fr1		/ -.5 < x < +.5
	mulf	fr1,fr1		/arg**2
/
	movf	P2,fr2
	mulf	fr1,fr2
	addf	P1,fr2
	mulf	fr1,fr2
	addf	P0,fr2
	mulf	fr2,fr0		/xP(x**2)
/
	movf	fr1,fr2
	addf	Q1,fr2
	mulf	fr1,fr2
	addf	Q0,fr2		/Q(x**2)
/
	movf	fr2,fr1
	subf	fr0,fr1
	addf	fr2,fr0
	divf	fr1,fr0		/(Q+xP)/(Q-xP)
	mulf	sqrt2,fr0
/
	stexp	fr0,-(sp)
	add	(sp)+,(sp)
/
	cmp	(sp),$177
	ble	2f
	tst	(sp)+
	movf	big,fr0		/overflow
	sec
	br	1f
2:
	cmp	(sp),$-177
	bge	2f
	tst	(sp)+
	clrf	fr0		/underflow
	clc
	br	1f
2:
	ldexp	(sp)+,fr0
	clc
1:
out:
	movf	(sp)+,fr1
	movf	(sp)+,fr2
	ldfps	(sp)+
	rts	pc
/
/
	.data
P0:	 42675;  36404;  77563;  46675
P1:	 41241; 116724; 114237;  60333
P2:	 36675;  27102; 125560; 136652
Q0:	 43210; 100661;  76072;  62453
Q1:	 42151;  27450;  75350; 112503
log2e:	 40270; 125073;  24534;  13761
sqrt2:	 40265;  02363;  31771; 157144
half	= 40000
one	= 40200
/
big:	 77777; 177777; 177777; 177777
/
/ P0 = .15139 06799 05433 89158 94328 d4
/ P1 = .20202 06565 12869 27227 886   d2
/ P2 = .23093 34775 37502 33624       d-1
/
/ Q0 = .43682 11662 72755 84984 96814 d4
/ Q1 = .23318 42114 27481 62379 0295  d3
/ Q2 = .1                             d1
/
/ log2e = 1.44269 50408 88963 40735 99246
/ sqrt2 = 1.41421 35623 73095 04880 16887
