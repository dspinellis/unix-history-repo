/ C library-- long divide/remainder

.globl	_ldiv, _ldivr
.globl	_lrem

_ldiv:
	mov	2(sp),r0
	mov	4(sp),r1
	div	6(sp),r0
	mov	r1,_ldivr
	rts	pc

_lrem:
	mov	2(sp),r0
	mov	4(sp),r1
	div	6(sp),r0
	mov	r1,r0
	rts	pc

.bss
_ldivr:	.=.+2
