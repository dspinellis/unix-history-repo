/
/

/  a7 -- pdp-11 assembler

expres:
	clr	xsymbol
expres1:
	mov	r5,-(sp)
	mov	$'+,-(sp)
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
	tst	r0
	bne	1f
	tstb	passno
	beq	1f
	jsr	r5,error; 'u
1:
	cmp	r0,$40
	bne	1f
	mov	r4,xsymbol
	clr	r1
	br	oprand
1:
	mov	2(r4),r1
	br	oprand
7:
	cmp	r4,$141
	blo	1f
	asl	r4
	mov	curfb-[2*141](r4),r0
	mov	2(r0),r1
	movb	(r0),r0
	br	oprand
1:
	mov	$esw1,r1
1:
	cmp	(r1)+,r4
	beq	1f
	tst	(r1)+
	bne	1b
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
	2;	exnum1
	'!;	binop
	200;	0

binop:
	cmpb	(sp),$'+
	beq	1f
	jsr	pc,errore
1:
	movb	r4,(sp)
	br	advanc

exnum1:
	mov	numval,r1
	br	1f

exnum:
	jsr	pc,getw
	mov	r4,r1
1:
	mov	$1,r0
	br	oprand

brack:
	mov	r2,-(sp)
	mov	r3,-(sp)
	jsr	pc,readop
	jsr	pc,expres1
	cmp	r4,$']
	beq	1f
	jsr	r5,error; ']
1:
	mov	r3,r0
	mov	r2,r1
	mov	(sp)+,r3
	mov	(sp)+,r2

oprand:
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
	'^; excmbin
	'!; exnot
	200;  0

excmbin:
	mov	r0,r3
	br	eoprnd

exrsh:
	neg	r1
	beq	exlsh
	inc	r1
	clc
	ror	r2
exlsh:
	jsr	r5,combin; relte2
	als	r1,r2
	br	eoprnd

exmod:
	jsr	r5,combin; relte2
	mov	r3,r0
	mov	r2,r3
	clr	r2
	dvd	r1,r2
	mov	r3,r2
	mov	r0,r3
	br	eoprnd

exadd:
	jsr	r5,combin; reltp2
	add	r1,r2
	br	eoprnd

exsub:
	jsr	r5,combin; reltm2
	sub	r1,r2
	br	eoprnd

exand:
	jsr	r5,combin; relte2
	com	r1
	bic	r1,r2
	br	eoprnd

exor:
	jsr	r5,combin; relte2
	bis	r1,r2
	br	eoprnd

exmul:
	jsr	r5,combin; relte2
	mpy	r2,r1
	mov	r1,r2
	br	eoprnd

exdiv:
	jsr	r5,combin; relte2
	mov	r3,r0
	mov	r2,r3
	clr	r2
	dvd	r1,r2
	mov	r0,r3
	br	eoprnd

exnot:
	jsr	r5,combin; relte2
	com	r1
	add	r1,r2
	br	eoprnd

eoprnd:
	mov	$'+,(sp)
	jmp	advanc

combin:
	tstb	passno
	bne	combin1
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
	cmp	(r5)+,$reltm2
	bne	2f
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
combin1:
	mov	r1,-(sp)
	clr	maxtyp
	jsr	pc,maprel
	mov	r0,r1
	mpy	$6,r1
	mov	r3,r0
	jsr	pc,maprel
	add	(r5)+,r0
	add	r1,r0
	movb	(r0),r3
	bpl	1f
	cmp	r3,$-1
	beq	2f
	jsr	r5,error; 'r
2:
	mov	maxtyp,r3
1:
	mov	(sp)+,r1
	rts	r5

maprel:
	cmp	r0,$40
	bne	1f
	mov	$5,r0
	rts	pc
1:
	bic	$!37,r0
	cmp	r0,maxtyp
	blos	1f
	mov	r0,maxtyp
1:
	cmp	r0,$5
	blos	1f
	mov	$1,r0
1:
	rts	pc

X = -2
M = -1
reltp2:
	.byte 0, 0, 0, 0, 0, 0
	.byte 0, M, 2, 3, 4,40
	.byte 0, 2, X, X, X, X
	.byte 0, 3, X, X, X, X
	.byte 0, 4, X, X, X, X
	.byte 0,40, X, X, X, X

reltm2:
	.byte 0, 0, 0, 0, 0, 0
	.byte 0, M, 2, 3, 4,40
	.byte 0, X, 1, X, X, X
	.byte 0, X, X, 1, X, X
	.byte 0, X, X, X, 1, X
	.byte 0, X, X, X, X, X

relte2:
	.byte 0, 0, 0, 0, 0, 0
	.byte 0, M, X, X, X, X
	.byte 0, X, X, X, X, X
	.byte 0, X, X, X, X, X
	.byte 0, X, X, X, X, X
	.byte 0, X, X, X, X, X

