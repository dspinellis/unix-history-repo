/
/

/ f23 -- do equivalence statements

.globl	equiv

.globl	getsym
.globl	consub
.globl	eqvtab
.globl	error
.globl	declimpl
.globl	perror
.globl	setln
.globl	getln

/ equivalence statements, part 1
/ destroys all registers

equiv:
	jsr	r5,setln
1:
	jsr	r5,getln
		rts r5
	cmp	r0,$'e
	bne	1b
	mov	$line+11.,r1
	mov	r5,-(sp)
2:				/ start equivalence group
	cmpb	(r1)+,$'(		/ check (
	bne	9f			/ syntax error
	jsr	r5,getsym
	tst	r0
	bne	9f			/ not identifier
	mov	r3,r5
	jsr	r5,equset
	movb	(r1)+,r2
	clr	r4			/ offset
	cmp	r2,$',
	beq	3f
	cmp	r2,$'(			/ subscripted vble
	bne	9f			/ syntax error
	jsr	r5,consub		/ get subscript
	mov	r0,r4
	cmpb	(r1)+,$',
	bne	9f
3:					/ rest of group
	jsr	r5,getsym		/ next ident
	tst	r0
	bne	9f			/ syntax
	jsr	r5,equset
	clr	r0
	mov	r3,r2
	cmpb	(r1),$'(		/ subscript?
	bne	4f
	inc	r1
	jsr	r5,consub
4:
	mov	eqvtab+2(r2),r2
	cmp	r2,r5
	beq	5f			/ already in same group
	cmp	r2,r3
	bne	4b			/ not yet in different group
	sub	r4,r0			/ adjust offsets
	sub	eqvtab+4(r5),r0		/ left vble's offset
	add	eqvtab+4(r3),r0		/ new vble's offset
4:
	sub	r0,eqvtab+4(r2)
	mov	eqvtab+2(r2),r2
	cmp	r2,r3
	bne	4b
	mov	eqvtab+2(r3),r0		/ link up groups
	mov	eqvtab+2(r5),eqvtab+2(r3)
	mov	r0,eqvtab+2(r5)		/ link groups
	br	6f
5:					/ here already in same group
	cmp	r0,r4			/ offset must be same
	beq	6f
	jsr	r5,error; 23.		/ inconsistency!
6:
	movb	(r1)+,r0
	cmp	r0,$',
	beq	3b
	cmp	r0,$')
	bne	9f
	movb	(r1)+,r0
	bne	3f
	jsr	r5,perror
	mov	(sp)+,r5
	br	1b
3:
	cmp	r0,$',
	beq	2b
9:
	jsr	r5,error; 24.		/ equivalence syntax
	jsr	r5,perror
	mov	(sp)+,r5
	br	1b

/ initialize member of equivalence group

equset:
	jsr	r5,declimpl		/ declare if necessary
	mov	symtab(r3),r0
	bit	$200,r0			/ test parameter
	bne	2f
	bic	$!70,r0
	cmp	r0,$10			/ simple
	beq	1f
	cmp	r0,$20			/ array
	beq	1f
2:
	jsr	r5,error;  31.		/ non-equivalencable variable
1:
	tst	eqvtab+2(r3)		/ see if mentioned yet
	bne	1f
	mov	r3,eqvtab+2(r3)		/ points to itself
1:
	rts	r5

