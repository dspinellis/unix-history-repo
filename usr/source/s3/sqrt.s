ldfps = 170100^tst
stfps = 170200^tst
/
/	sqrt replaces the f.p. number in fr0 by its
/	square root.  newton's method
/
.globl	sqrt
/
/
sqrt:
	tstf	fr0
	cfcc
	bne	1f
	clc
	rts	r5		/sqrt(0)
1:
	bgt	1f
	clrf	fr0
	sec
	rts	r5		/ sqrt(-a)
1:
	mov	r0,-(sp)
	stfps	-(sp)
	mov	(sp),r0
	bic	$!200,r0		/ retain mode
	ldfps	r0
	movf	fr1,-(sp)
	movf	fr2,-(sp)
/
	movf	fr0,fr1
	movf	fr0,-(sp)
	asr	(sp)
	add	$20100,(sp)
	movf	(sp)+,fr0	/initial guess
	mov	$4,r0
1:
	movf	fr1,fr2
	divf	fr0,fr2
	addf	fr2,fr0
	mulf	$half,fr0	/ x = (x+a/x)/2
	sob	r0,1b
2:
	movf	(sp)+,fr2
	movf	(sp)+,fr1
	ldfps	(sp)+
	mov	(sp)+,r0
	clc
	rts	r5
/
half	= 40000
