/
/

/ f15 -- subroutine/function

.globl	ssubr
.globl	sfunc
.globl	sbloc
.globl	sfunc1
.globl	funok

.globl	getsym
.globl	error

ssubr:
	mov	$2,progt
	br	1f

sfunc:
	mov	$4,progt
1:
	clr	r0
	br	2f

sfunc1:
	mov	$4,progt
2:
	tst	funok
	beq	1f
	jsr	r5,error; 11.
1:
	add	$10,r0		/ class=simple
	mov	r0,-(sp)
	jsr	r5,getsym
	mov	(sp)+,r2
	tst	r0
	beq	1f
2:
	jsr	r5,error; 12.
	rts	r5
1:
	mov	r2,symtab(r3)
	jsr	r5,getsym
	cmp	r0,$32.		/ (
	beq	1f
	cmp	r0,$40.	/ eos
	bne	2b
	rts	r5
1:
	clr	-(sp)
1:
	add	$2,(sp)
	jsr	r5,getsym
	tst	r0
	beq	3f
	tst	(sp)+
	br	2b
3:
	bis	$200,symtab(r3)	/ param
	mov	(sp),symtab+6(r3)	/ param number
	jsr	r5,getsym
	cmp	r0,$36.	/ ,
	beq	1b
	tst	(sp)+
	cmp	r0,$34.		/ )
	bne	2b
	jsr	r5,getsym
	cmp	r0,$40.
	bne	2b
	rts	r5

sbloc:
	mov	$6,progt
	jsr	r5,getsym
	tst	funok
	beq	1f
	jsr	r5,error; 11.
1:
	cmp	r0,$40.
	beq	1f
	jsr	r5,error; 13.
1:
	rts	r5

.data
funok:	0
