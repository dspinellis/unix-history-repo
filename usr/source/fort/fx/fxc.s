/
/

/ fxc -- size of array

.globl	size
.globl	nelem

.globl	error

size:
	movb	symtab+1(r3),r0
	jsr	r5,nelem
	inc	r0
	bic	$1,r0		/ round to 0 mod 2
	rts	r5

nelem:
	mov	r1,-(sp)
	mov	r0,r1
	mov	symtab(r3),r0
	bic	$!70,r0
	cmp	r0,$20
	bne	1f
	mov	symtab+2(r3),r0
	mov	(r0)+,-(sp)
2:
	mpy	(r0)+,r1
	dec	(sp)
	bgt	2b
	tst	(sp)+
1:
	mov	r1,r0
	mov	(sp)+,r1
	rts	r5

