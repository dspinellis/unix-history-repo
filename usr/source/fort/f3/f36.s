/
/

/ f36 -- expression code generation

.globl	lvalue
.globl	rvalue
.globl	convrt
.globl	type
.globl	funcappl
.globl	name

.globl	error
.globl	lookup
.globl	code
.globl	iserror
.globl	genop
.globl	typ
.globl	newline
.globl	functn
.globl	size

lvalue:
	jsr	r5,iserror
		rts r5
	tst	(r2)
	bne	1f
	mov	2(r2),r3
	jsr	r5,code
		<	lval>; .even
	br	name
1:
	mov	r2,-(sp)
	cmp	(r2),$32.		/ array appl
	bne	1f
	jsr	r5,aryappl
	dec	r0
	bne	2f			/ dim =| 1
	tstb	symtab(r3)
	blt	2f			/ p-bit, not simple
	mov	$"al,r0
	br	simpary
2:
	jsr	r5,code
		<	alval\0>; .even

arydope:
	jsr	r5,pbit
	mov	symtab+2(r3),r2
	mov	(r2)+,r0
	asl	r0
	add	r0,r2
	mov	(r2),r0
	jsr	r5,code
		<; d%d\0>; .even
		r0
	br	2f
1:
	jsr	r5,error; 54.
	mov	(sp)+,r2
	rts	r5

simpary:
	mov	r3,-(sp)
	mov	symtab(r3),r3
	bis	$7,r3
	jsr	r5,genop
	mov	(sp)+,r3
	jsr	r5,size
	jsr	r5,code
		<; %d.\0>; .even
		r0
	br	2f

name:
	mov	r2,-(sp)
	jsr	r5,pbit
2:
	jsr	r5,code
		<; \0>; .even
	bit	$100,symtab(r3)		/ common
	beq	1f
	mov	symtab+4(r3),r2
	jsr	r5,code
		<%n+\0>; .even
		r2
1:
	movb	symtab(r3),r2
	bic	$!70,r2
	cmp	r2,$30			/ external
	bne	1f
	jsr	r5,code
		<%n.\n\0>; .even
		r3
	br	2f
1:
	jsr	r5,code
		<%n_\n\0>; .even
		r3
2:
	mov	symtab(r3),r3
	mov	(sp)+,r2
	rts	r5

rvalue:
	jsr	r5,iserror
		rts r5
	mov	r2,-(sp)
	tst	(r2)
	bne	1f
	mov	2(r2),r3
	movb	symtab+1(r3),r2
	jsr	r5,code
		<	rval%d>; .even
		r2
	mov	(sp)+,r2
	br	name
1:
	cmp	(r2),$32.
	bne	1f
	jsr	r5,aryappl
	dec	r0
	bne	3f
	tstb	symtab(r3)
	blt	3f
	mov	$"ar,r0
	br	simpary
3:
	jsr	r5,code
		<	arval\0>; .even
	br	arydope
1:
	cmp	(r2),$34.		/ array appl
	bne	1f
	jsr	r5,funcappl
	mov	(sp)+,r2
	mov	2(r2),r3
	movb	symtab+1(r3),r0
	jsr	r5,code
		<%d.\n\0>; .even
		r0
	mov	symtab(r3),r3
	rts	r5
1:
	cmp	(r2),$2
	bne	1f
	movb	3(r2),r3
	mov	4(r2),r2
	jsr	r5,code
		<	rval%d; c%d\n\0>; .even
		r3
		r2
	mov	(sp)+,r2
	mov	2(r2),r3
	rts	r5
1:
	cmp	(r2),$24.		/ arith or relat
	bhi	1f
	mov	2(r2),r2
	bne	3f
	mov	(sp),r2
	sub	$10.,(r2)		/ - bin -> - unary
	mov	4(r2),r2
	jsr	r5,rvalue
	br	2f
3:
	jsr	r5,rvalue
	mov	(sp),r2
	mov	r3,-(sp)
	mov	4(r2),r2
	jsr	r5,type
	cmp	*2(sp),$4			/ **
	bne	3f
	mov	r3,r2
	bic	$!7,r2
	cmp	r2,$1		/ ** integer
	bne	3f
	mov	2(sp),r2
	sub	$2,(r2)		/ pr -> pi
	mov	4(r2),r2
	jsr	r5,rvalue
	mov	$intcon,r2
	jsr	r5,convrt
	mov	(sp)+,r3
	br	2f
3:
	mov	(sp),r2
	jsr	pc,maxtyp
	mov	(sp)+,r3
	mov	r2,-(sp)
	jsr	r5,convrt
	mov	2(sp),r2
	mov	4(r2),r2
	jsr	r5,rvalue
	mov	(sp)+,r2
	jsr	r5,convrt
	mov	r2,r3
	br	2f
1:
	cmp	(r2),$30.		/ and or not
	bhi	1f
	mov	2(r2),r2
	beq	3f
	jsr	r5,rvalue
	mov	$logcon,r2
	jsr	r5,convrt
3:
	mov	(sp),r2
	mov	4(r2),r2
	jsr	r5,rvalue
	mov	$logcon,r2
	jsr	r5,convrt
2:
	mov	(sp)+,r2
	mov	(r2),r0
	cmp	r0,$4
	bhis	2f
	add	$10.,(r2)		/ back to binary
	tst	r0
	beq	2f
	sub	$8,(r2)		/ back to pr
2:
	mov	optab(r0),r0
	jsr	r5,genop
	jsr	r5,newline
	cmp	(r2),$14.		/ relat
	blo	2f
	mov	$logcon,r3
2:
	rts	r5
1:
	jsr	r5,error; 54.
	mov	(sp)+,r2
	rts	r5

pbit:
	tstb	symtab(r3)
	bge	1f
	jsr	r5,code
		<p\0>
1:
	rts	r5

funcappl:
	mov	r2,-(sp)
	mov	functn,r3
	jsr	r5,code
		<	stsp; ft+%d.\n\0>; .even
		r3
	mov	r3,-(sp)
	add	$2,r3
	mov	r3,functn
	clr	-(sp)		/ nargs
1:
	mov	4(r2),r2
	beq	2f
	inc	(sp)
	cmp	(r2),$36.	/ ,
	bne	1f
	mov	r2,-(sp)
	mov	2(r2),r2
	mov	6(sp),r3
	jsr	r5,fapp1
	mov	(sp)+,r2
	br	1b
1:
	mov	4(sp),r3
	jsr	r5,fapp1
2:
	mov	(sp)+,r0		/ nargs
	mov	(sp)+,r2
	mov	(sp)+,r3
	mov	2(r3),r3
	jsr	r5,code
		<	call\0>; .even
	jsr	r5,pbit
	jsr	r5,code
		<; %n.; ft+%d.; %d.; \0>; .even
		r3
		r2
		r0
	cmp	functn,functm
	ble	1f
	mov	functn,functm
1:
	mov	r2,functn
	rts	r5

fapp1:
	mov	2(r3),r3		/ fetch out function name
	mov	symtab+2(r3),r3		/ arg conversion
	bne	2f
	tst	(r2)
	beq	1f
	cmp	(r2),$32.
	beq	4f
	cmp	(r2),$42.		/ lv if funct or name or arry app
	beq	1f
	cmp	(r2),$2			/ lv if const
	bne	2f
	mov	4(r2),r3
	jsr	r5,code
		<	lval; c%d\n\0>
		r3
	br	3f
2:
	mov	r3,-(sp)
	jsr	r5,rvalue
	mov	(sp)+,r2
	beq	2f
	jsr	r5,convrt
2:
	mov	functn,r3
	jsr	r5,code
		<	stsp; ft+%d.\n\0>; .even
		r3
	add	$2,functn
	rts	r5
1:
	clr	(r2)			/ turn func/array names into lvs
4:
	jsr	r5,lvalue
3:
	mov	functn,r3
	jsr	r5,code
		<	stst; ft+%d.\n\0>; .even
		r3
	add	$2,functn
	rts	r5

aryappl:
	mov	r2,-(sp)
	clr	-(sp)		/ arg count
2:
	inc	(sp)
	mov	4(r2),r2
	cmp	(r2),$36.	/ ,
	bne	2f
	mov	r2,-(sp)
	mov	2(r2),r2
	jsr	r5,rvalue
	mov	$intcon,r2
	jsr	r5,convrt
	mov	(sp)+,r2
	br	2b
2:
	jsr	r5,rvalue
	mov	$intcon,r2
	jsr	r5,convrt
	mov	(sp)+,r0
	mov	(sp)+,r2
	mov	2(r2),r3
	cmp	r0,*symtab+2(r3)
	beq	1f
	jsr	r5,error; 53.		/ dimension mismatch
1:
	rts	r5

/ converts stack from type in r3 to type in r2
convrt:
	mov	r2,-(sp)
	mov	r3,-(sp)
	bic	$![377\<8+7],r2
	bic	$![377\<8+7],r3
	cmp	r2,r3
	beq	1f
	jsr	r5,code
		<	\0>; .even
	jsr	pc,2f
	mov	r2,r3
	jsr	pc,2f
	jsr	r5,code
		<\n\0>; .even
1:
	mov	(sp)+,r3
	mov	(sp)+,r2
	rts	r5
2:
	mov	r2,-(sp)
	mov	r3,r2
	clrb	r3
	swab	r3
	bic	$!7,r2
	movb	typ(r2),r2
	jsr	r5,code
		<%c%d\0>; .even
		r2
		r3
	mov	(sp)+,r2
	rts	pc

type:
	cmp	(r2),$32.
	beq	2f
	cmp	(r2),$34.
	beq	2f
	tst	(r2)
	bne	1f
2:
	mov	2(r2),r3
	mov	symtab(r3),r3
	rts	r5
1:
	cmp	(r2),$2
	bne	1f
	mov	2(r2),r3
	rts	r5
1:
	cmp	(r2),$14.
	blo	1f
	mov	$logcon,r3
	rts	r5
1:
	mov	r2,-(sp)
	mov	2(r2),r2
	bne	1f
	mov	(sp),r2
	mov	4(r2),r2
	jsr	r5,type
	br	2f
1:
	jsr	r5,type
	mov	(sp),r2
	mov	r3,-(sp)
	mov	4(r2),r2
	jsr	r5,type
	mov	(sp)+,r2
	jsr	pc,maxtyp
	mov	r2,r3
2:
	mov	(sp)+,r2
	rts	r5

maxtyp:
	mov	r2,r0
	cmp	r2,r3
	bhis	2f
	mov	r3,r2
2:
	clrb	r2
	bic	$!7,r0
	bic	$!7,r3
	cmp	r0,r3
	bhis	2f
	mov	r3,r0
2:
	bis	r0,r2
	rts	pc

optab:
	<ng>
	<pi>
	<pr>
	<dv>
	<mp>
	<sb>
	<ad>
	<lt>
	<le>
	<eq>
	<ne>
	<gt>
	<ge>
	<nt>
	<an>
	<or>

