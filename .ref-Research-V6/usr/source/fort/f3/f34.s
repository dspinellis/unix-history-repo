/
/

/ f34 -- if statement

.globl	sif

.globl	e2
.globl	error
.globl	rvalue
.globl	code
.globl	ifstmt
.globl	iserror
.globl	getsym
.globl	geti
.globl	genop
.globl	getlab
.globl	newline

sif:
	jsr	r5,e2			/ expression
	jsr	r5,iserror
		br 9f
	cmp	r0,$34.			/ )
	bne	9f
	jsr	r5,rvalue
	mov	$"if,r0
	jsr	r5,genop
	bic	$!7,r3
	cmp	r3,$3			/ logical
	bne	1f
	jsr	r5,code
		<; 1f\n\0>; .even
	jsr	r5,ifstmt
	jsr	r5,code
		<1:\n\0>; .even
	rts	r5
1:
	mov	$3,-(sp)
1:
	jsr	r5,getlab
		br 8f
	jsr	r5,code
		<; .%d\0>; .even
		r0
	dec	(sp)
	beq	1f
	jsr	r5,getsym
	cmp	r0,$36.
	beq	1b
8:
	tst	(sp)+
	br	9f
1:
	tst	(sp)+
	jsr	r5,newline
	jsr	r5,getsym
	cmp	r0,$40.
	bne	9f
	rts	r5

9:
	jsr	r5,error; 40.
	rts	r5

