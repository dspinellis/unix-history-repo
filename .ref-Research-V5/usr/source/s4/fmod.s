.globl	_fmod, retrn
_fmod:
	mov	r5,-(sp)
	mov	sp,r5
	movf	4(r5),fr0
	divf	12.(r5),fr0
	modf	$one,fr0
	mulf	12.(r5),fr1
	movf	4(r5),fr0
	subf	fr1,fr0
	jmp	retrn
one = 40200
