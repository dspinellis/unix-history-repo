/
/

/ fxh -- array constant offset

.globl	consub

.globl	getsym
.globl	geti
.globl	error
.globl	geticon

/ turn constant subscripts into offset.
/	in: r3 -> symtab
/	    r1 -> just beyond (
/	out:r0 has offset

consub:
	mov	symtab(r3),r0
	bic	$!70,r0
	cmp	r0,$20		/ test class == array
	beq	1f
	jsr	r5,error; 17.
	clr	r0
	rts	r5
1:
	mov	r5,-(sp)
	mov	r4,-(sp)
	mov	r3,-(sp)
	mov	r2,-(sp)
	clr	r4		/ accumulated offset
	mov	symtab+2(r3),r2	/ ptr to declarator
	mov	(r2)+,-(sp)	/ dimensionality
	mov	$1,r5		/ prod of declarators
1:
	jsr	r5,geticon
		br 9f
	cmp	r0,(r2)
	bgt	3f
	dec	r0
	bge	2f
3:
	jsr	r5,error; 19.	/ out of range
	clr	r0
2:
	mov	r5,-(sp)
	mpy	r0,r5
	add	r5,r4
	mov	(sp)+,r5
	mpy	(r2)+,r5
	jsr	r5,getsym
	cmp	r0,$36.		/ comma
	bne	1f
	dec	(sp)
	bgt	1b
	jsr	r5,error; 18.	/ wrong subscript count
	br	1b
1:
	cmp	r0,$34.		/ )
	beq	1f
9:
	jsr	r5,error; 20.
	clr	r0
1:
	cmp	(sp)+,$1
	beq	1f
	jsr	r5,error; 18.	/ subscript count
1:
	mov	(sp)+,r2
	mov	(sp)+,r3
	movb	symtab+1(r3),r5
	mpy	r4,r5
	mov	r5,r0
	mov	(sp)+,r4
	mov	(sp)+,r5
	rts	r5

