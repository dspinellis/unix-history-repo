/ C register save and restore

.globl	rsave
.globl	mrsave
.globl	rretrn

mrsave:
	tst	(r5)+

rsave:
	mov	r5,r0
	mov	sp,r5
	mov	r4,-(sp)
	mov	r3,-(sp)
	mov	r2,-(sp)
	sub	(r0)+,sp
	jmp	(r0)

rretrn:
	sub	$6,r5
	mov	r5,sp
	mov	(sp)+,r2
	mov	(sp)+,r3
	mov	(sp)+,r4
	mov	(sp)+,r5
	rts	pc
