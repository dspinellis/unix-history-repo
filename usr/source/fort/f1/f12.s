/
/

/ f12 -- type statements and type getter subroutine
/
/

.globl	stype
.globl	getype
.globl	typtab

.globl	sfunc1
.globl	lookup
.globl	getsym
.globl	error
.globl	sdime1

stype:
	mov	r0,-(sp)
	mov	r1,r2
	jsr	r5,lookup; funtab
		br 1f
	mov	r2,r1
	mov	(sp)+,r0
	jmp	sfunc1
1:
	jsr	r5,getsym
	tst	r0
	bne	3f		/ junk error if not identifier
	mov	symtab(r3),r0
	bic	$![377\<8+7],r0 / size,type
	beq	2f
	cmp	r0,(sp)		/ redefined, but same
	beq	2f
	jsr	r5,error; 3.
2:
	bic	$377\<8+7,symtab(r3)
	bis	(sp),symtab(r3)	/ set in type and size
	mov	r3,-(sp)
	jsr	r5,getsym
	cmp	r0,$32.		/ ( for dimension
	bne	2f
	mov	(sp),r3
	jsr	r5,sdime1
	jsr	r5,getsym
2:
	tst	(sp)+
	cmp	r0,$36.		/ , for another list element
	beq	1b
	cmp	r0,$40.		/ eos
	beq	1f
3:
	jsr	r5,error; 2.	/ junk
1:
	tst	(sp)+
	rts	r5

funtab:
	<function\0>
	<\0>

getype:
	mov	r1,r2
	jsr	r5,lookup; typtab
		br 1f
	mov	tvaltab(r0),r0
	tst	(r5)+
1:
	rts	r5

tvaltab:
	log1con
	logcon
	int1con
	int2con
	intcon
	dcplxcon
	dblcon
	cplxcon
	dblcon
	realcon

typtab:
	<logical*1\0>
	<logical\0>
	<integer*1\0>
	<integer*2\0>
	<integer\0>
	<doublecomplex\0>
	<doubleprecision\0>
	<complex\0>
	<real*8\0>
	<real\0>
	<\0>
