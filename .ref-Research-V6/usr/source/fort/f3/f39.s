/
/

/ f39 -- i/o statements

.globl	sread
.globl	swrit
.globl	sprin
.globl	sback
.globl	srewi
.globl	sendf

.globl	getsym
.globl	error
.globl	geticon
.globl	e2
.globl	lvalue
.globl	rvalue
.globl	iserror
.globl	convrt
.globl	code
.globl	chrtab
.globl	blocks
.globl	blockp
.globl	doend
.globl	genop
.globl	levzer
.globl	dobeg
.globl	intexp
.globl	ptemp
.globl	blocks
.globl	blockp
.globl	intexp
.globl	newline
.globl	nelem

sprin:
	mov	$8.,-(sp)
	jsr	r5,getfmt
		br 9f
	cmp	r0,$36.			/ ,
	beq	1f
	cmp	r0,$40.			/ =|
	bne	8f
	clrb	(r1)
1:
	mov	$34.,r0			/ simulate )
	br	2f

sread:
	mov	$2,-(sp)
	br	1f

swrit:
	clr	-(sp)

1:
	cmpb	(r1)+,$'(
	bne	8f
	jsr	r5,intexp
	mov	$blocks,blockp
	cmp	r0,$34.			/ ), implies unformatted
	beq	2f
	cmp	r0,$36.			/ ,
	bne	8f
	jsr	r5,getfmt
		br 9f
	add	$4,(sp)
	cmp	r0,$34.			/ )
	bne	8f
2:
	mov	(sp),r0
	mov	iotype(r0),r0
	jsr	r5,code
		<	%s\n\0>; .even
		r0
1:
	tstb	(r1)
	beq	9f
1:
	jsr	r5,list
	cmp	r0,$40.			/ |=
	beq	9f
8:
	jsr	r5,error; 41.
9:
	jsr	r5,code
		<	endio\n\0>; .even
	tst	(sp)+
	rts	r5

iotype:
	1f
	2f
	3f
	4f
	5f

1:
	<iowu\0>
2:
	<ioru\0>
3:
	<iowf\0>
4:
	<iorf\0>
5:
	<iowp\0>
	.even

getfmt:
	movb	(r1),r0
	cmpb	chrtab(r0),$4		/ digit
	beq	1f
	jsr	r5,e2
	jsr	r5,iserror
		rts r5
	mov	r0,-(sp)
	jsr	r5,lvalue
	mov	$blocks,blockp
	br	8f
1:
	jsr	r5,geticon
		br 8f
	mov	r0,temp
	jsr	r5,ptemp; 'i; temp; line	/ register use of format
	jsr	r5,code
		<	lval; .%d\n\0>; .even
		r0
	jsr	r5,getsym
	mov	r0,-(sp)
8:
	mov	(sp)+,r0
	tst	(r5)+
	rts	r5

list:
	jsr	r5,lstitm
	cmp	r0,$36.			/ ,
	beq	list
	rts	r5

lstitm:
	mov	$blocks,blockp
	cmpb	(r1),$'(		/ test for sublist
	beq	1f
	jsr	r5,e2
	jsr	r5,iserror
		rts r5
	mov	r0,-(sp)
	clr	-(sp)
	tst	(r2)
	bne	2f			/ test for name
	mov	2(r2),r3
	mov	symtab(r3),r0
	bic	$!70,r0
	cmp	r0,$20			/ test for short list
	bne	2f
	mov	pc,(sp)
	mov	symtab+2(r3),r3
	mov	(r3)+,-(sp)
	asl	(sp)
	add	(sp)+,r3
	mov	(r3),r3
	jsr	r5,code
		<	slist1; d%d\n2:\0>; .even
		r3
2:
	jsr	r5,lvalue
	tst	(sp)
	beq	3f
	jsr	r5,code
		<	slist3\n\0>; .even
3:
	mov	$"io,r0
	jsr	r5,genop
	tst	(sp)+
	beq	2f
	jsr	r5,code
		<\n	slist2; 2b\0>; .even
2:
	jsr	r5,newline
	mov	(sp)+,r0
	rts	r5
1:
	inc	r1
	jsr	r5,levzer; '=
		br  1f			/ yes, implied do
	jsr	r5,list
	jsr	r5,chkel
	jsr	r5,getsym
	rts	r5
1:
	cmp	r1,r0
	bhis	8f
	cmpb	-(r0),$',		/ look backwards
	bne	1b
	mov	r0,-(sp)
	mov	r1,-(sp)
	movb	$'),(r0)		/ fake!!
	mov	r0,r1
	inc	r1
	clr	r0
	jsr	r5,dobeg		/ get do
	jsr	r5,chkel
	mov	(sp)+,r0
	mov	r1,-(sp)
	mov	r0,r1
	jsr	r5,list
	jsr	r5,chkel
	clr	r0
	jsr	r5,doend
	mov	(sp)+,r1
	movb	$',,*(sp)+		/ unfake!!
	jsr	r5,getsym
	rts	r5

chkel:
	cmp	r0,$34.			/ )
	beq	1f
8:
	jsr	r5,error; 41.
1:
	rts	r5

sback:
	mov	$bksp,r2
	br	1f

srewi:
	mov	$rewi,r2
	br	1f

sendf:
	mov	$enfl,r2

1:
	mov	r2,-(sp)
	jsr	r5,intexp
	mov	(sp),r2
	jsr	r5,code
		<	%s\n\0>; .even
		r2
	cmp	r0,$40.			/ =|
	beq	9f
8:
	jsr	r5,error; 41.
9:
	tst	(sp)+
	rts	r5

bksp:
	<bksp\0>
rewi:
	<rewi\0>
enfl:
	<enfl\0>

