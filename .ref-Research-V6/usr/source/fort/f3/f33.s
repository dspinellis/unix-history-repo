/
/

/ f33 -- do, etal

.globl	sdo
.globl	dobeg
.globl	doend

.globl	getlab
.globl	e2
.globl	error
.globl	lvalue
.globl	rvalue
.globl	code
.globl	convrt
.globl	dou
.globl	dotabp
.globl	edotab
.globl	dotab

sdo:
	jsr	r5,getlab
		br 9f
	jsr	r5,dobeg
	cmp	r0,$40.
	bne	9f
	rts	r5

7:
	tst	(sp)+
8:
	tst	(sp)+
9:
	jsr	r5,error; 42.
	rts	r5

dobeg:
	mov	dotabp,r2
	cmp	r2,$edotab
	blo	1f
	jsr	r5,error; 44.
	rts	r5
1:
	mov	dou,dotab(r2)
	inc	dou
	mov	r0,dotab+2(r2)
	add	$4,dotabp
	jsr	r5,e2
	cmp	r0,$38.		/ =
	bne	9b
	mov	r2,-(sp)
	jsr	r5,lvalue
	mov	r3,-(sp)
	bic	$!7,r3
	cmp	r3,$1		/ integer
	bne	7b
	jsr	r5,e2
	cmp	r0,$36.		/ ,
	bne	7b
	jsr	r5,rvalue
	mov	(sp),r2
	jsr	r5,convrt
	mov	(sp),r3
	clrb	r3
	swab	r3
	jsr	r5,code
		<	gas%d\n	goto; 2f\n\0>; .even
		r3
	mov	dotabp,r2
	mov	dotab-4(r2),r0
	jsr	r5,code
		<t%d:\n\0>; .even
		r0
	mov	(sp)+,r3
	mov	(sp)+,r2
	mov	r0,-(sp)
	mov	r3,-(sp)
	jsr	r5,lvalue
	jsr	r5,e2
	mov	r0,-(sp)
	jsr	r5,rvalue
	mov	2(sp),r2
	jsr	r5,convrt
	cmp	(sp)+,$36.		/ ,
	bne	1f
	jsr	r5,e2
	mov	r0,-(sp)
	jsr	r5,rvalue
	mov	2(sp),r2
	jsr	r5,convrt
	mov	(sp)+,r0
	jsr	r5,code
		<	do2\0>; .even
	br	2f
1:
	jsr	r5,code
		<	do1\0>; .even
2:
	mov	(sp)+,r3
	clrb	r3
	swab	r3
	mov	(sp)+,r2
	jsr	r5,code
		<%d; o%d\n2:\n\0>; .even
		r3
		r2
	rts	r5

doend:
	tst	r0
	bne	1f
	jsr	pc,gen
	rts	r5
1:
	clr	r2
1:
	cmp	r2,dotabp
	bhis	1f
	add	$4,r2
	cmp	r0,dotab-2(r2)
	bne	1b
	jsr	pc,gen
	br	doend
1:
	rts	r5

gen:
	mov	r0,-(sp)
	mov	dotabp,r2
	cmp	r0,dotab-2(r2)
	beq	1f
	jsr	r5,error; 43.
1:
	mov	dotab-4(r2),r0
	sub	$4,dotabp
	jsr	r5,code
		<	goto; t%d\no%d:\n\0>; .even
		r0
		r0
	mov	(sp)+,r0
	rts	pc

