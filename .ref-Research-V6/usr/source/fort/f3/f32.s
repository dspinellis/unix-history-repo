/
/

/ f32 -- expression to tree

.globl	e1
.globl	e2
.globl	e11

.globl	error
.globl	getsym
.globl	blockp
.globl	blocks
.globl	declimpl
.globl	conu
.globl	ptemp
.globl	functn
.globl	funimpl

/	e1:	e2[,e1]
/	e2:	e2[.or.e3]
/	e3:	e3[.and.e4]
/	e4:	[.not.]e5
/	e5:	e6[.rel.e6]
/	e6:	[+-]e7[+-e6]
/	e7:	e7[*/e8]
/	e8:	e9[**e9]
/	e9:	constant
/		name[(e1)]
/		(e2)
e1:
	jsr	r5,e2
	cmp	r0,$36.		/ ,
	bne	1f
	jsr	r5,block; e1
	rts	r5

e2:
	jsr	r5,e3
2:
	cmp	r0,$30.		/ or
	bne	1f
	jsr	r5,block; e3
	br	2b

e3:
	jsr	r5,e4
2:
	cmp	r0,$28.		/ and
	bne	1f
	jsr	r5,block; e4
	br	2b

e4:
	jsr	r5,getsym
	cmp	r0,$26.		/ not
	bne	e5
	clr	r2
	jsr	r5,block; e5a
	rts	r5

e5a:
	jsr	r5,getsym
e5:
	jsr	r5,e6
	cmp	r0,$14.	/ .lt.
	blo	1f
	cmp	r0,$24.	/ .ge.
	bhi	1f
	jsr	r5,block; e6a
	rts	r5

e6a:
	jsr	r5,getsym
e6:
	cmp	r0,$12.		/ +
	beq	e6a
	cmp	r0,$10.		/ -
	bne	2f
	clr	r2
	jsr	r5,block; e7a
	br	3f
2:
	jsr	r5,e7
3:
	cmp	r0,$10.		/ -
	blo	1f
	cmp	r0,$12.		/ +
	bhi	1f
	jsr	r5,block; e7a
	br	3b

e7a:
	jsr	r5,getsym
e7:
	jsr	r5,e8
2:
	cmp	r0,$6.		/ /
	blo	1f
	cmp	r0,$8.		/ *
	bhi	1f
	jsr	r5,block; e8a
	br	2b

e8a:
	jsr	r5,getsym
e8:
	jsr	r5,e9
	cmp	r0,$4		/ **
	bne	1f
	jsr	r5,block; e9a
1:
	rts	r5

e9a:
	jsr	r5,getsym
e9:
	cmp	r0,$2		/ constant
	beq	3f
/ (e2)
	cmp	r0,$32.		/ (
	bne	1f
	jsr	r5,e2
	br	2f
e10:
	jsr	r5,e1
2:
	cmp	r0,$34.	/ )
	beq	2f
	jsr	r5,error; 29.
	rts	r5
2:
	jsr	r5,getsym
	rts	r5
1:
	tst	r0
	beq	e11
/ unknown
	jsr	r5,error; 30.
	rts	r5

/ name
e11:
	mov	r3,r2
	jsr	r5,getsym
	cmp	r0,$32.		/ (
	bne	1f
	jsr	r5,appl
	jsr	r5,block; e10
	rts	r5
1:
	mov	r2,r3
	mov	r0,-(sp)
	jsr	r5,declimpl
	mov	symtab(r3),r0
	bic	$!70,r0
	cmp	r0,$30			/ class =| funct
	bne	1f
	jsr	r5,appl
	mov	$42.,r3			/ just function name
	br	2f
1:
	clr	r3
2:
	mov	(sp)+,r0
	mov	r2,-(sp)
	mov	blockp,r2
	add	$6,blockp
	mov	r3,(r2)
	mov	(sp)+,2(r2)
	clr	4(r2)
	rts	r5

/ number
3:
	mov	blockp,r2
	add	$6,blockp
	mov	r0,(r2)
	mov	r3,2(r2)
	cmpb	2(r2),$5		/ const->int
	bne	4f
	movb	$1,2(r2)
4:
	mov	r3,temp
	mov	conu,4(r2)
	inc	conu
	jsr	r5,ptemp; 'c; temp; symbuf
	jsr	r5,getsym
	rts	r5

appl:
	clr	functn
	bit	$70,symtab(r2)		/ class
	bne	1f
	bis	$30,symtab(r2)
	jsr	r5,funimpl
1:
	mov	r2,r3
	jsr	r5,declimpl
	mov	symtab(r2),r0
	bic	$!70,r0			/ class again
	cmp	r0,$20		/ array
	beq	1f
	cmp	r0,$30		/ funct
	beq	2f
	jsr	r5,error; 33.
	bic	$70,symtab(r2)
	br	appl
1:
	mov	$32.,r0
	rts	r5
2:
	mov	$34.,r0
	rts	r5

block:
	mov	blockp,r3
	add	$6,blockp
	mov	r0,(r3)+
	mov	r2,(r3)+
	mov	r3,-(sp)
	jsr	r5,*(r5)+
	mov	r2,*(sp)
	mov	(sp)+,r2
	sub	$4,r2
	rts	r5

