/
/

/  a7 -- pdp-11 assembler pass 1

expres:
	mov	r5,-(sp)
	mov	$'+,-(sp)
	clr	opfound
	clr	r2
	mov	$1,r3
	br	1f
advanc:
	jsr	pc,readop
1:
	mov	r4,r0
	jsr	r5,betwen; 0; 177
		br .+4
	br	7f
	movb	(r4),r0
	mov	2(r4),r1
	br	oprand
7:
	cmp	r4,$141
	blo	1f
	cmp	r4,$141+10.
	bhis	2f
	movb	curfbr-141(r4),r0
	asl	r4
	mov	curfb-[2*141](r4),r2
	bpl	oprand
	jsr	r5,error; 'f
	br	oprand
2:
	clr	r3
	clr	r2
	br	oprand
1:
	mov	$esw1,r1
1:
	cmp	(r1)+,r4
	beq	1f
	tst	(r1)+
	bne	1b
	tst	opfound
	bne	2f
	jsr	pc,errore
2:
	tst	(sp)+
	mov	(sp)+,r5
	rts	pc
1:
	jmp	*(r1)

esw1:
	'+;	binop
	'-;	binop
	'*;	binop
	'/;	binop
	'&;	binop
	037;	binop
	035;	binop
	036;	binop
	'%;	binop
	'[;	brack
	'^;	binop
	1;	exnum
	'!;	binop
	0;	0

binop:
	cmpb	(sp),$'+
	beq	1f
	jsr	pc,errore
1:
	movb	r4,(sp)
	br	advanc

exnum:
	mov	numval,r1
	mov	$1,r0
	br	oprand

brack:
	mov	r2,-(sp)
	mov	r3,-(sp)
	jsr	pc,readop
	jsr	pc,expres
	cmp	r4,$']
	beq	1f
	jsr	r5,error; ']
1:
	mov	r3,r0
	mov	r2,r1
	mov	(sp)+,r3
	mov	(sp)+,r2

oprand:
	inc	opfound
	mov	$exsw2,r5
1:
	cmp	(sp),(r5)+
	beq	1f
	tst	(r5)+
	bne	1b
	br	eoprnd
1:
	jmp	*(r5)

exsw2:
	'+; exadd
	'-; exsub
	'*; exmul
	'/; exdiv
	037; exor
	'&; exand
	035;exlsh
	036;exrsh
	'%; exmod
	'!; exnot
	'^; excmbin
	0;  0

excmbin:
	mov	r0,r3			/ give left flag of right
	br	eoprnd

exrsh:
	neg	r1
	beq	exlsh
	inc	r1
	clc
	ror	r2
exlsh:
	jsr	r5,combin; 0
	als	r1,r2
	br	eoprnd

exmod:
	jsr	r5,combin; 0
	mov	r1,-(sp)
	mov	r2,r1
	clr	r0
	dvd	(sp)+,r0
	mov	r1,r2
	br	eoprnd

exadd:
	jsr	r5,combin; 0
	add	r1,r2
	br	eoprnd

exsub:
	jsr	r5,combin; 1
	sub	r1,r2
	br	eoprnd

exand:
	jsr	r5,combin; 0
	com	r1
	bic	r1,r2
	br	eoprnd

exor:
	jsr	r5,combin; 0
	bis	r1,r2
	br	eoprnd

exmul:
	jsr	r5,combin; 0
	mpy	r2,r1
	mov	r1,r2
	br	eoprnd

exdiv:
	jsr	r5,combin; 0
	mov	r1,-(sp)
	mov	r2,r1
	clr	r0
	dvd	(sp)+,r0
	mov	r0,r2
	br	eoprnd

exnot:
	jsr	r5,combin; 0
	com	r1
	add	r1,r2
	br	eoprnd

eoprnd:
	mov	$'+,(sp)
	jmp	advanc

combin:
	mov	r0,-(sp)
	bis	r3,(sp)
	bic	$!40,(sp)
	bic	$!37,r0
	bic	$!37,r3
	cmp	r0,r3
	ble	1f
	mov	r0,-(sp)
	mov	r3,r0
	mov	(sp)+,r3
1:
	tst	r0
	beq	1f
	tst	(r5)+
	beq	2f
	cmp	r0,r3
	bne	2f
	mov	$1,r3
	br	2f
1:
	tst	(r5)+
	clr	r3
2:
	bis	(sp)+,r3
	rts	r5

