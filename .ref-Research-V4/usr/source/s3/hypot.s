/ hypot -- sqrt(fr0**2 + fr1**2) -> fr0; no overflow unless
/ necessary.
/ c-bit is set on overflow

.globl	hypot
.globl	sqrt

hypot:
	movf	fr1,-(sp)
	absf	fr0
	absf	fr1
	cmpf	fr0,fr1
	cfcc
	bne	1f
	movf	$one,fr0
	br	3f
1:
	blt	2f
	movf	fr0,-(sp)
	movf	fr1,fr0
	movf	(sp)+,fr1
2:
	divf	fr1,fr0
	mulf	fr0,fr0
3:
	addf	$one,fr0
	jsr	r5,sqrt
	mulf	fr1,fr0
	cfcc
	movf	(sp)+,fr1
	rts	r5

one = 40200
