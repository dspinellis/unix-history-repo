/ C register save and restore -- version 12/74

.globl	csv
.globl	cret

csv:
	mov	r5,r0
	mov	sp,r5
	mov	r4,-(sp)
	mov	r3,-(sp)
	mov	r2,-(sp)
	tst	-(sp)
	jmp	(r0)

cret:
	mov	r5,r1
	mov	-(r1),r4
	mov	-(r1),r3
	mov	-(r1),r2
	mov	r5,sp
	mov	(sp)+,r5
	rts	pc
