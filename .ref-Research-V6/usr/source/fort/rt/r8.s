/
/

/ r8 - fortran runtime -- powers of integers

.globl	ipi2

ipi2:
	tst	(sp)+
	mov	(sp)+,r0
	bge	1f
	clr	(sp)
	jmp	*(r4)+
1:
	mov	(sp)+,r1
	mov	r3,-(sp)
	mov	$1,r3
1:
	tst	r0
	beq	1f
	asr	r0
	bcc	2f
	mpy	r1,r3
2:
	mpy	r1,r1
	br	1b
1:
	mov	r3,r1
	mov	(sp)+,r3
	mov	r1,-(sp)
	jmp	*(r4)+

