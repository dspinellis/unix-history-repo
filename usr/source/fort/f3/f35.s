/
/

/ f35 -- assignment statement

.globl	sasgn
.globl	sassi

.globl	error
.globl	e2
.globl	lvalue
.globl	rvalue
.globl	code
.globl	convrt
.globl	geti
.globl	getsym
.globl	getlab
.globl	genop
.globl	newline
.globl	name

sasgn:
	jsr	r5,e2
	cmp	r0,$38.		/ =
	bne	9f
	mov	r2,-(sp)
	jsr	r5,e2
	cmp	r0,$40.		/ =|
	bne	8f
	tst	*(sp)
	bne	1f			/ too hard for simple as
	jsr	r5,rvalue
	mov	(sp)+,r2
	mov	2(r2),r2
	mov	r2,-(sp)
	mov	symtab(r2),r2
	jsr	r5,convrt
	mov	r2,r3
	bis	$7,r3
	mov	$"mv,r0
	jsr	r5,genop
	mov	(sp)+,r3
	jsr	r5,name
	rts	r5
1:
	mov	r2,r3
	mov	(sp)+,r2
	mov	r3,-(sp)
	jsr	r5,lvalue
	mov	(sp)+,r2
	mov	r3,-(sp)
	jsr	r5,rvalue
1:
	mov	(sp)+,r2
	jsr	r5,convrt
	mov	r2,r3
	bis	$7,r3		/ type 'g'
	mov	$"as,r0
	jsr	r5,genop
	jsr	r5,newline
	rts	r5

sassi:
	jsr	r5,getlab
		br 9f
	mov	r0,-(sp)
	cmpb	(r1)+,$'t
	bne	8f
	cmpb	(r1)+,$'o
	bne	8f
	jsr	r5,e2
	cmp	r0,$40.			/ =|
	bne	8f
	jsr	r5,lvalue
	mov	(sp)+,r0
	mov	r3,-(sp)
	jsr	r5,code
		<	lval; .%d\n\0>; .even
		r0
	mov	$int2con,r3
	br	1b
8:
	tst	(sp)+
9:
	jsr	r5,error; 39.
	rts	r5

