.globl	pow, log, exp

pow:

/ 0^0~ is 0

	tstf	fr0
	cfcc
	bne	1f
	tstf	fr1
	cfcc
	beq	bad
	rts	r5
1:

/ -^i is +^i fixed sign

	bpl	1f
	movf	fr1,-(sp)
	modf	$one,fr1
	cfcc
	bne	bad1
	movf	(sp)+,fr1
	absf	fr0
	jsr	r5,1f
	bes	bad
	movf	fr1,-(sp)
	modf	$half,fr1
	cfcc
	beq	2f
	negf	fr0
2:
	movf	(sp)+,fr1
	rts	r5
1:
	jsr	r5,log
	bes	1f
	mulf	 fr1,fr0
	jsr	r5,exp
1:
	rts	r5

bad1:
	movf	(sp)+,fr1
bad:
	sec
	rts	r5

one = 40200
half = 40000
