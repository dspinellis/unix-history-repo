/
/

/ f14 -- common statement
/

.globl	scomm

.globl	getsym
.globl	sdime1
.globl	error
.globl	getid
.globl	lookid

scomm:
	mov	r5,-(sp)
	jsr	r5,getsym
	cmp	r0,$6		/ / for named common
	beq	ncom
3:
	tst	r0
	bne	9f
	mov	r3,-(sp)
	clr	r5
	br	2f

ncom:
	cmpb	(r1),$'/
	bne	3f
	inc	r1
	jsr	r5,getsym
	br	3b
3:
	jsr	r5,getid
		br 9f		/ not identifier
	jsr	r5,lookid; symbuf-1
	bis	$40,symtab(r3)	/ set named common
	mov	r3,r5
	jsr	r5,getsym
	cmp	r0,$6		/ /
	bne	9f
1:
	jsr	r5,getsym
	tst	r0
	bne	9f
	mov	r3,-(sp)
2:
	jsr	r5,getsym
	cmp	r0,$32.		/ (
	bne	2f
	mov	(sp),r3
	jsr	r5,sdime1
	jsr	r5,getsym
2:
	mov	(sp)+,r3
	bit	$300,symtab(r3)	/ test param/common
	beq	2f
	jsr	r5,error; 9.	/ not commonable or already commoned
	br	3f
2:
	mov	r0,-(sp)
	mov	symtab+4(r5),r0
	beq	2f
	mov	r3,symtab+4(r0)	/ next ptr of old last block
2:
	mov	(sp)+,r0
	mov	r3,symtab+4(r5)	/ new last ptr of head block
	tst	symtab+2(r5)
	bne	3f
	mov	r3,symtab+2(r5)	/ first ptr if first block
3:
	bis	$100,symtab(r3)
	cmp	r0,$36.		/ ,
	beq	1b
	cmp	r0,$6		/ another /
	beq	ncom
	cmp	r0,$40.		/ =|
	beq	8f
9:
	jsr	r5, error; 10.	/ common syntax error
8:
	mov	(sp)+,r5
	rts	r5


