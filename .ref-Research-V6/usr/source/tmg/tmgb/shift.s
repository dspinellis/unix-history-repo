.globl .p,sprv
.globl .sr,.sl

/ >>
.sr:
	neg	(sp)

/ <<
.sl:
	mov	4(sp),r1
	clr	r0
	alsc	(sp),r0
	mov	r1,4(sp)
	jmp	.p
