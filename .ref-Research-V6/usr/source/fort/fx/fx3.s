/
/

/ fx3 -- get integer

.globl	geti
.globl	ptemp

.globl	putc
.globl	putw

geti:
	mov	r1,-(sp)
	mov	r3,-(sp)
	clr	r3
	mov	$symbuf,r0
1:
	movb	(r0)+,r1
	sub	$'0,r1
	cmp	r1,$9
	bhi	1f
	mpy	$10.,r3
	add	r1,r3
	br	1b
1:
	mov	r3,r0
	mov	(sp)+,r3
	mov	(sp)+,r1
	rts	r5

ptemp:
	mov	r0,-(sp)
	mov	r1,-(sp)
	mov	(r5)+,r0		/ character
	jsr	r5,putc; tbuf
	mov	*(r5)+,r0		/ word
	jsr	r5,putw; tbuf
	mov	ifno,r0
	jsr	r5,putw; tbuf
	mov	(r5)+,r1		/ string
1:
	movb	(r1),r0
	jsr	r5,putc; tbuf
	tstb	(r1)+
	bne	1b
	mov	(sp)+,r1
	mov	(sp)+,r0
	rts	r5

