/
/

/ 42 -- definition

.globl	bsss

.globl	code
.globl	size
.globl	declimpl
.globl	cdata
.globl	dodata
.globl	dattab

bsss:
	jsr	r5,cdata
	cmp	progt,$6		/ test block common
	bne	1f
	rts	r5
1:
	jsr	r5,code
		<.bss\n\0>; .even
	mov	functm,r3
	beq	1f
	jsr	r5,code
		<ft:	.=.+%d.\n\0>; .even
		r3
1:
	cmp	r4,$dattab			/ any data?
	beq	1f
	jsr	r5,code
		<.data\n\0>; .even
1:
	jsr	r5,code
		<base:\n\0>; .even
	clr	r3
1:
	cmp	r3,symtp
	bhis	pass2
	bit	$70,symtab(r3)			/ test classed
	bne	2f
	jsr	r5,declimpl
2:
	mov	symtab(r3),r0
	mov	symtab+6(r3),r2
	bic	$!70,r0			/ class
	cmp	r0,$10			/ simple
	beq	1f
	cmp	r0,$20			/ array
	beq	1f
	cmp	r0,$30			/ extrn
	bne	2f
	bit	$200,symtab(r3)		/ param
	beq	2f
	jsr	r5,code
		<%n.	= %d.\n\0>; .even
		r3
		r2
2:
	cmp	r0,$40			/ common block
	bne	2f
	mov	symtab+6(r3),r2		/ size
	beq	2f
	jsr	r5,code
		<.comm	%n,%d.\n\0>; .even
		r3
		r2
2:
	add	$8,r3
	br	1b

1:
	bit	$300,symtab(r3)
	beq	1f
	jsr	r5,code
		<%n_	= %d.\n\0>; .even
		r3
		r2
	br	2b
1:
	tst	r2
	bne	1f
	jsr	r5,size
	mov	nxtaloc,r2
	mov	r2,symtab+6(r3)
	add	r0,nxtaloc
1:
	jsr	r5,code
		<%n_ = base+%d.\n\0>; .even
		r3
		r2
	br	2b

pass2:
	jsr	r5,dodata
	rts	r5

