/
/ FORS
/
_FOR1U:
	mov	(sp)+,r2
	movb	(r2),r1
	sxt	r0
	incb	(r2)
	br	1f
_FOR2U:
	mov	(sp)+,r2
	mov	(r2),r1
	sxt	r0
	inc	(r2)
	br	1f
_FOR4U:
	mov	(sp)+,r2
	mov	(r2)+,r0
	mov	(r2),r1
	add	$1,(r2)
	adc	-2(r2)
1:
	mov	(sp)+,r2
	mov	(sp)+,r3
	cmp	r0,r2
	beq	1f
	bgt	2f
	add	(lc),lc
	return
1:
	cmp	r1,r3
	bhis	2f
	add	(lc),lc
	return
2:
	tst	(lc)+
	return
_FOR1D:
	mov	(sp)+,r2
	movb	(r2),r1
	sxt	r0
	decb	(r2)
	br	1f
_FOR2D:
	mov	(sp)+,r2
	mov	(r2),r1
	sxt	r0
	dec	(r2)
	br	1f
_FOR4D:
	mov	(sp)+,r2
	mov	(r2)+,r0
	mov	(r2),r1
	sub	$1,(r2)
	sbc	-2(r2)
1:
	mov	(sp)+,r2
	mov	(sp)+,r3
	cmp	r0,r2
	beq	1f
	blt	2f
	add	(lc),lc
	return
1:
	cmp	r1,r3
	blos	2f
	add	(lc),lc
	return
2:
	tst	(lc)+
	return
