.globl	_floor, _ceil
.globl	floor, ceil
one = 40200

_floor:
	mov	r5,-(sp)
	mov	sp,r5
	movf	4(r5),fr0
	jsr	pc,floor
	br	1f

_ceil:
	mov	r5,-(sp)
	mov	sp,r5
	movf	4(r5),fr0
	jsr	pc,ceil
1:
	mov	(sp)+,r5
	rts	pc

floor:
	tstf	fr0
	cfcc
	bge	1f
	modf	$one,fr0
	cfcc
	beq	2f
	subf	$one,fr1
	br	2f
1:
	modf	$one,fr0
2:
	movf	fr1,fr0
	rts	pc

ceil:
	negf	fr0
	jsr	pc,floor
	negf	fr0
	rts	pc
