/
/

one = 40200
/  r9 -- fortran runtime -- real**integer

.globl	ipi4
.globl	rpi4
.globl	rpi8

.globl	rerr

ipi4:
	setf
	setl
	movif	4(sp),fr0
	movf	fr0,4(sp)
	jsr	r5,1f
	movfi	fr1,-(sp)
	jmp	*(r4)+

rpi4:
	setf
	br	2f

rpi8:
	setd
2:
	jsr	r5,1f
	movf	fr1,-(sp)
	jmp	*(r4)+

1:
	cmp	(sp)+,(sp)+		/ jsr + msp of i4
	mov	(sp)+,r0
	movf	$one,fr1
	movf	(sp)+,fr0
	tst	r0
	bgt	1f
	cfcc
	beq	9f
	tst	r0
	beq	3f
	divf	fr0,fr1
	movf	fr1,fr0
	neg	r0
	movf	$one,fr1
1:
	asr	r0
	bcc	2f
	mulf	fr0,fr1
2:
	tst	r0
	beq	3f
	mulf	fr0,fr0
	br	1b
3:
	jmp	(r5)

9:
	jsr	r5, rerr; 17.
