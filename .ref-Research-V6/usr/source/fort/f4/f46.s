/
/

/ f46 -- constant evaluation

.globl	getcon
.globl	dope

.globl	code

getcon:
	mov	r0,-(sp)
	mov	r2,-(sp)
	clr	r2			/ dec . counter
	clrf	fr0
	movif	$10.,fr1
	clr	-(sp)			/ - flag
	cmpb	(r1)+,$'+
	beq	1f
	cmpb	-(r1),$'-
	bne	1f
	inc	r1
	inc	(sp)
1:
	movb	(r1)+,r0
	sub	$'0,r0
	cmp	r0,$9
	blos	2f
	cmp	r0,$'.-'0
	bne	1f
	dec	r2
	br	1b
2:
	tst	r2
	beq	2f
	dec	r2
2:
	mulf	fr1,fr0
	movif	r0,fr2
	addf	fr2,fr0
	br	1b
1:
	tst	r2
	bne	1f
	dec	r2
1:
	cmp	r0,$'e-'0
	beq	2f
	cmp	r0,$'d-'0
	bne	1f
2:
	mov	r3,-(sp)
	clr	r3
	clr	-(sp)
	cmpb	(r1),$'-
	bne	3f
	inc	r1
	inc	(sp)
	br	2f
3:
	cmpb	(r1),$'+
	bne	2f
	inc	r1
2:
	movb	(r1)+,r0
	sub	$'0,r0
	cmp	r0,$9
	bhi	2f
	mpy	$10.,r3
	add	r0,r3
	br	2b
2:
	tst	(sp)+
	beq	2f
	neg	r3
2:
	add	r3,r2
	mov	(sp)+,r3
1:
	movf	fr1,fr2
	add	$1,r2
	beq	1f
	blt	2f
	clr	-(sp)
	br	3f
2:
	mov	pc,-(sp)
	neg	r2
3:
	sub	$1,r2
	ble	2f
	mulf	fr1,fr2
	br	3b
2:
	tst	(sp)+
	bne	2f
	mulf	fr2,fr0
	br	1f
2:
	divf	fr2,fr0
1:
	tst	(sp)+
	beq	1f
	negf	fr0
1:
	cmpb	-(r1),$',
	bne	1f
	movf	fr0,-(sp)
	inc	r1
	jsr	r5,getcon
	movf	(sp)+,fr1		/ a,b -> r1,r0
1:
	mov	(sp)+,r2
	mov	(sp)+,r0
	rts	r5

dope:
	cmp	progt,$6		/ test "block data"
	bne	1f
	rts	r5
1:
	clr	r3
1:
	cmp	r3,symtp
	bhis	1f
	mov	symtab(r3),r0
	bic	$!70,r0
	cmp	r0,$20
	bne	2f
	mov	symtab+2(r3),r0
	beq	2f
	mov	(r0)+,r1
	mov	r1,r2
	asl	r2
	add	r0,r2
	mov	(r2),r0
	jsr	r5,code
		<d%d:	%d.\n	\0>; .even
		r0
		r1
3:
	dec	r1
	blt	3f
	mov	-(r2),r0
	bge	4f
	jsr	r5,code
		<..; \0>; .even
	br	3b
4:
	jsr	r5,code
		<%d.; \0>; .even
		r0
	br	3b
3:
	mov	symtab(r3),r2
	clrb	r2
	swab	r2
	jsr	r5,code
		<%d.\n\0>; .even
		r2
2:
	add	$8,r3
	br	1b
1:
	rts	r5

