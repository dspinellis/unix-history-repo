/ C library -- switch

.globl	bswitch

bswitch:
	mov	*(sp)+,r1
1:
	cmp	(r1)+,r0
	beq	1f
	tst	(r1)+
	bne	1b
2:
	mov	-4(r1),pc
1:
	mov	(r1)+,r0
	beq	2b
	mov	r0,pc

