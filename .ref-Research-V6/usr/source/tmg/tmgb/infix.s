.globl .p,sprv
.globl .a,.s,.o,.n,.x

/ +
.a:
	jsr	pc,sprv
	add	(sp),4(sp)
	br	9f

/ -
.s:
	jsr	pc,sprv
	sub	(sp),4(sp)
	br	9f

/ |
.o:
	jsr	pc,sprv
	bis	(sp),4(sp)
	br	9f

/ &
.n:
	jsr	pc,sprv
	com	(sp)
	bic	(sp),4(sp)
	br	9f

/ ^ exclusive or
.x:
	jsr	pc,sprv
	mov	(sp),r0
	xor	r0,4(sp)
9:
	jmp	.p
