.globl	_floor, _ceil
.globl	floor, ceil
one = 40200

_floor:
	movf	2(sp),fr0

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

_ceil:
	movf	2(sp),fr0

ceil:
	negf	fr0
	jsr	pc,floor
	negf	fr0
	rts	pc
