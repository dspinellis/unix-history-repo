/
/

/ f38 -- wierdo statements

.globl	sstop
.globl	scall
.globl	spaus
.globl	sretu
.globl	scont
.globl	iserror

.globl	ptemp
.globl	code
.globl	geti
.globl	getsym
.globl	error
.globl	declimpl
.globl	e11
.globl	funcappl

sstop:
	clr	r0
	tstb	(r1)
	beq	1f
	jsr	r5,getsym
	cmp	r0,$2.			/ const
	bne	7f
	cmp	r3,$intcon
	bne	7f
	jsr	r5,geti
1:
	jsr	r5,code
		<	stop; %d\n\0>; .even
		r0
	br	8f

spaus:
	jsr	r5,getsym
	cmp	r0,$2
	bne	7f
	cmp	r3,$intcon
	bne	7f
	jsr	r5,geti
	jsr	r5,code
		<	paus; %d\n\0>; .even
		r0
	br	8f

sretu:
	tst	progt
	bne	1f
	jsr	r5,error;  37.		/ return in main
	br	sstop
1:
	jsr	r5,code
		<	retrn\n\0>; .even
8:
scont:
	tstb	(r1)
	bne	7f
	rts	r5
7:
	jsr	r5,error; 38. 
	rts	r5

iserror:
	cmp	errp,$errb
	bne	1f
	tst	(r5)+
1:
	rts	r5

scall:
	jsr	r5,getsym
	tst	r0
	bne	7b
	bit	$70,symtab(r3)
	bne	1f
	bis	$30,symtab(r3)
1:
	jsr	r5,declimpl
	jsr	r5,e11
	cmp	r0,$40.
	bne	7b
	cmp	(r2),$34.
	beq	1f
	cmp	(r2),$42.
	bne	7b
1:
	jsr	r5,funcappl
	jsr	r5,code
		<0\n\0>; .even
	br	8b

