/ Character list get/put

ps	= 0177776
spl	= 230

.globl	_getc
.globl	_putc

.globl	_cfreelist

_getc:
	mov	2(sp),r1
	mov	ps,-(sp)
	spl	5
	mov	2(r1),r2	/ first ptr
	beq	9f		/ empty
	movb	(r2)+,r0	/ character
	bic	$!377,r0
	mov	r2,2(r1)
	dec	(r1)+		/ count
	bne	1f
	clr	(r1)+
	clr	(r1)+		/ last block
	br	2f
1:
	bit	$7,r2
	bne	3f
	mov	-10(r2),(r1)	/ next block
	add	$2,(r1)
2:
	dec	r2
	bic	$7,r2
	mov	_cfreelist,(r2)
	mov	r2,_cfreelist
3:
	mov	(sp)+,ps
	rts	pc
9:
	clr	4(r1)
	mov	$-1,r0
	mov	(sp)+,ps
	rts	pc

_putc:
	mov	2(sp),r0
	mov	4(sp),r1
	mov	ps,-(sp)
	mov	r2,-(sp)
	mov	r3,-(sp)
	spl	5
	mov	4(r1),r2	/ last ptr
	bne	1f
	mov	_cfreelist,r2
	beq	9f
	mov	(r2),_cfreelist
	clr	(r2)+
	mov	r2,2(r1)	/ first ptr
	br	2f
1:
	bit	$7,r2
	bne	2f
	mov	_cfreelist,r3
	beq	9f
	mov	(r3),_cfreelist
	mov	r3,-10(r2)
	mov	r3,r2
	clr	(r2)+
2:
	movb	r0,(r2)+
	mov	r2,4(r1)
	inc	(r1)		/ count
	clr	r0
	mov	(sp)+,r3
	mov	(sp)+,r2
	mov	(sp)+,ps
	rts	pc
9:
	mov	pc,r0
	mov	(sp)+,r3
	mov	(sp)+,r2
	mov	(sp)+,ps
	rts	pc
