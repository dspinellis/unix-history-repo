/ abs - int absolute value.
/ fabs - floating abs

.globl	_abs
_abs:
	mov	2(sp),r0
	bge	1f
	neg	r0
1:
	rts	pc

.globl	_fabs
_fabs:
	movf	2(sp),fr0
	absf	fr0
	rts	pc
