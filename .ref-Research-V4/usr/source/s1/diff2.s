.globl	_readhash
.globl getc
h = 0
s = 2

.data

_readhash:
	mov	2(sp),0f
	mov	r5,-(sp)
	sub	$4,sp
	clr	h(sp)
	clr	s(sp)
1:
	jsr	r5,getc; 0: 0
	bes	2f
	bic	$177400,r0
	cmp	r0,$'\n
	beq	1f
	ashc	$-16.,r0
	ashc	s(sp),r0
	add	r0,h(sp)
	adc	h(sp)
	add	r1,h(sp)
	adc	h(sp)
	add	$7,s(sp)
	cmp	s(sp),$16.
	blt	1b
	sub	$16.,s(sp)
	br	1b
1:
	clr	r0
	cmp	h(sp),$-1
	beq	1f
	mov	h(sp),r0
1:
	cmp	(sp)+,(sp)+
	mov	(sp)+,r5
	rts	pc
2:
	mov	$-1,r0
	br	1b
