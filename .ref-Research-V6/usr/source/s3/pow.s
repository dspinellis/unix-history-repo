.globl	pow, _pow
.globl	log, exp

_pow:
	mov	r5,-(sp)
	mov	sp,r5
	movf	4(r5),fr0
	movf	12.(r5),fr1
	jsr	pc,pow
	mov	(sp)+,r5
	rts	pc

pow:

/ 0^0~ is 0

	tstf	fr0
	cfcc
	bne	1f
	tstf	fr1
	cfcc
	beq	bad
	rts	pc
1:

/ -^i is +^i fixed sign

	bpl	1f
	movf	fr1,-(sp)
	modf	$one,fr1
	cfcc
	bne	bad1
	movf	(sp)+,fr1
	absf	fr0
	jsr	pc,1f
	bes	bad
	movf	fr1,-(sp)
	modf	$half,fr1
	cfcc
	beq	2f
	negf	fr0
2:
	movf	(sp)+,fr1
	rts	pc
1:
	jsr	pc,log
	bes	1f
	mulf	 fr1,fr0
	jsr	pc,exp
1:
	rts	pc

bad1:
	movf	(sp)+,fr1
bad:
	sec
	rts	pc

one = 40200
half = 40000
