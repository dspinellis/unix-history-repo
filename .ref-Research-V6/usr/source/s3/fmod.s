/ C floating mod
/ fmod(a,b) returns a number n such that
/	i*b + n is a and 0 <= n < b

.globl	_fmod
_fmod:
	mov	r5,-(sp)
	mov	sp,r5
	movf	4(r5),fr0
	divf	12.(r5),fr0
	modf	$one,fr0
	mulf	12.(r5),fr1
	movf	4(r5),fr0
	subf	fr1,fr0
	mov	(sp)+,r5
	rts	pc
one = 40200
