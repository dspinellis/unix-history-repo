/ C library-- simple switch

.globl	sswitch

/	jsr	pc,sswitch
/	L; V
/	...
/	0; default

sswitch:
	mov	(sp)+,r1
	mov	r2,-(sp)
1:
	mov	(r1)+,r2
	beq	1f
	cmp	(r1)+,r0
	bne	1b
	rts	r2
1:
	mov	(sp)+,r2
	jmp	*(r1)+
