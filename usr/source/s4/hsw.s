/ C library-- hash switch

.globl	hswitch

/	jsr	pc,hswitch; size; table; default
/	p0
/	0
/	...
/p0:	L; V
/	...
/	0

hswitch:
	mov	(sp)+,r1
	mov	r2,-(sp)
	mov	r3,-(sp)
	mov	r0,r3
	clr	r2
	div	(r1)+,r2
	asl	r3
	add	(r1)+,r3
	mov	(r3),r3
	beq	1f
2:
	mov	(r3)+,r2
	beq	1f
	cmp	(r3)+,r0
	bne	2b
	mov	(sp)+,r3
	rts	r2
1:
	mov	(sp)+,r3
	mov	(sp)+,r2
	jmp	*(r1)+
