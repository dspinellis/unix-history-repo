.globl .m,.q,.r
.globl .p,sprv

.m:
	jsr	pc,sprv
	mov	(sp),r0
	mpy	4(sp),r0
	mov	r1,4(sp)
	br	1f

.q:
	jsr	pc,sprv
	mov	4(sp),r1
	sxt	r0
	dvd	(sp),r0
	mov	r0,4(sp)
	br	1f

.r:
	jsr	pc,sprv
	mov	4(sp),r1
	sxt	r0
	dvd	(sp),r0
	mov	r1,4(sp)
1:
	jmp	.p
