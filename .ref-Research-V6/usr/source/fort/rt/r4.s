/
/

/ r4 -- short integer arithmetic

.globl	iad2
.globl	isb2
.globl	imp2
.globl	idv2
.globl	i2i4
.globl	i1i4
.globl	i4i2
.globl	i4i1
.globl	ing2
.globl	rval2
.globl	rval1
.globl	l2l1

iad2:
	add	(sp)+,(sp)
	jmp	*(r4)+

isb2:
	sub	(sp)+,(sp)
	jmp	*(r4)+

imp2:
	mov	(sp)+,r1
	mul	(sp)+,r1
	mov	r1,-(sp)
	jmp	*(r4)+

idv2:
	mov	2(sp),r1
	sxt	r0
	div	(sp)+,r0
	mov	r0,(sp)
	jmp	*(r4)+

i4i2:
i4i1:
	tst	(sp)+
	jmp	*(r4)+

i2i4:
i1i4:
	tst	(sp)
	sxt	-(sp)
	jmp	*(r4)+

ing2:
	neg	(sp)
	jmp	*(r4)+

rval2:
	mov	*(r4)+,-(sp)
	jmp	*(r4)+

rval1:
	movb	*(r4)+,r0
	mov	r0,-(sp)

l2l1:
	jmp	*(r4)+

