/
/

/ f13 -- dimension and array declarator
/
/	dimension
/
/
.globl	sdime
.globl	sdime1

.globl	getsym
.globl	error
.globl	geti
.globl	dimu

sdime:
	jsr	r5,getsym
	tst	r0
	bne	3f		/ junk error
	mov	r3,-(sp)
	jsr	r5,getsym
	mov	(sp)+,r3
	cmp	r0,$32.		/ (
	beq	2f
	jsr	r5,error; 4.	/ no ( in dimension
	rts	r5
2:
	jsr	r5,sdime1
	jsr	r5,getsym
	cmp	r0,$36.		/ ,
	beq	sdime
	cmp	r0,$40.
	beq	1f
3:
	jsr	r5,error; 5.
1:
	rts	r5

/ get dimension info and store in
/ symbol table entry
/ r3 points at symbol table
/ r1 points just beyond (
sdime1:
	bit	$70,symtab(r3)	/ class
	beq	1f
	jsr	r5,error; 6.	/ already classed
1:
	mov	r3,-(sp)
	bic	$70,symtab(r3)
	bis	$20,symtab(r3)	/ set as array
	clr	-(sp)		/ marker
1:
	jsr	r5,getsym
	cmp	r0,$2		/ constant
	bne	2f
	cmp	r3,$intcon	/ integer*4
	bne	3f
	jsr	r5,geti
	tst	r0
	ble	3f
	mov	r0,-(sp)
	br	4f
2:
	tst	r0
	bne	3f		/ identifer
	tstb	symtab(r3)
	bge	3f		/ not parameter
	neg	r3
	mov	r3,-(sp)
	mov	sp,r0
2:
	tst	(r0)+
	bne	2b
	mov	(r0),r0
	tstb	symtab(r0)
	bge	3f		/ array not a param
4:
	jsr	r5,getsym
	cmp	r0,$36.		/ ,
	beq	1b
	cmp	r0,$34.		/ )
	beq	1f
3:
	jsr	r5,error; 7.
1:
	mov	esymp,r0
	clr	r2
	mov	dimu,-(r0)
	inc	dimu		/ unique number
1:
	inc	r2
	mov	(sp)+,-(r0)
	bne	1b
	dec	r2
	mov	r2,(r0)
	mov	r0,esymp
	mov	(sp)+,r3
	mov	r0,symtab+2(r3)
	rts	r5

