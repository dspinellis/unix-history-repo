/
/

/ f22 -- allocate common

.globl	calloc
.globl	entry

.globl	declimpl
.globl	size
.globl	getc
.globl	getw
.globl	xbuf
.globl	code
.globl	typ

calloc:
	clr	r3
1:
	cmp	r3,symtp
	bhis	1f
	mov	symtab(r3),r0
	bic	$!70,r0		/ class
	cmp	r0,$40		/ common block
	bne	3f
	mov	r3,-(sp)
	mov	symtab+2(r3),r3
	clr	r2		/ byte offset
2:
	tst	r3
	beq	2f
	jsr	r5,declimpl
	mov	symtab+4(r3),-(sp)
	mov	2(sp),symtab+4(r3)
	mov	r2,symtab+6(r3)
	jsr	r5,size
	add	r0,r2
	mov	(sp)+,r3
	br	2b
2:
	mov	(sp)+,r3
	clr	symtab+2(r3)
	mov	r2,symtab+6(r3)		/ common block size
3:
	add	$8,r3
	br	1b
1:
	rts	r5

entry:
	mov	progt,r0
	jmp	*1f(r0)
1:
	main
	subr
	funct
	blocd

main:
	jsr	r5,code
		<main:\n\0>; .even
	rts	r5

subr:
funct:
	jsr	r5,code
		<%n.:	%n_\n\0>; .even
		8
		8
	clr	r3
1:
	cmp	r3,symtp
	bhis	1f
	mov	symtab+2(r3),r0
	beq	2f
	mov	(r0)+,r1	/ num dims
	asl	r1
	add	r0,r1		/ ptr to last dim
	mov	r3,-(sp)
	mov	(r1),-(sp)	/ dope id
	clr	r2		/ dope offset
3:
	add	$2,r2
	mov	-(r1),r3
	cmp	r0,r1
	bhi	3f
	neg	r3		/ adjustable dimension
	ble	3b
	mov	r0,-(sp)
	jsr	r5,declimpl
	mov	symtab(r3),r0
	clrb	r0
	swab	r0
	jsr	r5,code
		<	rval%dp; %n_\n\0>; .even
		r0
		r3
	mov	symtab(r3),r3
	bic	$![377\<8+7],r3
	cmp	r3,$2\<8+1		/ is it i*2
	beq	4f
	bic	$!7,r3
	movb	typ(r3),r3
	jsr	r5,code
		<	%c%di2\n\0>; .even
		r3
		r0
4:
	mov	2(sp),r0
	jsr	r5,code
		<	stst; d%d+%d.\n\0>; .even
		r0
		r2
	mov	(sp)+,r0
	br	3b
3:
	tst	(sp)+
	mov	(sp)+,r3
2:
	add	$8,r3
	br	1b
1:
blocd:
	rts	r5

