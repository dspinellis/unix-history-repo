/
/

/ a6 -- pdp-11 assembler pass 1

opline:
	mov	r4,r0
	jsr	r5,betwen; 0; 200
		br	1f
	cmp	r0,$'<
	bne	xpr
	jmp	opl17
xpr:
	jsr	pc,expres
	add	$2,dot
	rts	pc
1:
	movb	(r4),r0
	cmp	r0,$24
	beq	xpr
	jsr	r5,betwen; 5; 36
		br xpr
	mov	r0,-(sp)
	jsr	pc,readop
	mov	(sp)+,r0
	asl	r0
	jmp	*1f-12(r0)

1:
	opl13	/ map fop freg,fdst to double
	opl6
	opl7
	opl10
	opl11
	opl13	/ map fld/fst to double
	opl13
	opl13	/ map fop fsrc,freg to double
	opl15
	opl16
	opl17
	opl20
	opl21
	opl22
	opl23
	xpr
	opl25
	opl26
	opl27
	opl13  / map mul s,r to double
	opl31
	opl32
	xpr
	xpr
	opl35
	opl36

/ jbr
opl35:
	mov	$4,-(sp)
	br	1f

/ jeq, etc
opl36:
	mov	$6,-(sp)
1:
	jsr	pc,expres
	cmp	r3,dotrel
	bne	1f
	sub	dot,r2
	bge	1f
	cmp	r2,$-376
	blt	1f
	mov	$2,(sp)
1:
	add	(sp)+,dot
	rts	pc

/double
opl13:
opl7:
	jsr	pc,addres
op2:
	cmp	r4,$',
	beq	1f
	jsr	pc,errora
	rts	pc
1:
	jsr	pc,readop
opl15:   / single operand
	jsr	pc,addres
	add	$2,dot
	rts	pc

opl31:	/ sob
	jsr	pc,expres
	cmp	r4,$',
	beq	1f
	jsr	pc,errora
1:
	jsr	pc,readop

/branch
opl6:
opl10:
opl11:
	jsr	pc,expres
	add	$2,dot
	rts	pc

/ .byte
opl16:
	jsr	pc,expres
	inc	dot
	cmp	r4,$',
	bne	1f
	jsr	pc,readop
	br	opl16
1:
	rts	pc

/ < (.ascii)
opl17:
	add	numval,dot
	jsr	pc,readop
	rts	pc

/.even
opl20:
	inc	dot
	bic	$1,dot
	rts	pc

/.if
opl21:
	jsr	pc,expres
	tst	r3
	bne	1f
	jsr	r5,error; 'U
1:
	tst	r2
	bne	opl22
	inc	ifflg
opl22:	/endif
	rts	pc

/.globl
opl23:
	cmp	r4,$200
	blo	1f
	bisb	$40,(r4)
	jsr	pc,readop
	cmp	r4,$',
	bne	1f
	jsr	pc,readop
	br	opl23
1:
	rts	pc

opl25:
opl26:
opl27:
	mov	dotrel,r1
	asl	r1
	mov	dot,savdot-4(r1)
	mov	savdot-[2*25](r0),dot
	asr	r0
	sub	$25-2,r0
	mov	r0,dotrel
	rts	pc

/ .common
opl32:
	cmp	r4,$200
	blo	1f
	bis	$40,(r4)
	jsr	pc,readop
	cmp	r4,$',
	bne	1f
	jsr	pc,readop
	jsr	pc,expres
	rts	pc
1:
	jsr	r5,error; 'x
	rts	pc

addres:
	cmp	r4,$'(
	beq	alp
	cmp	r4,$'-
	beq	amin
	cmp	r4,$'$
	beq	adoll
	cmp	r4,$'*
	beq	astar
getx:
	jsr	pc,expres
	cmp	r4,$'(
	bne	2f
	jsr	pc,readop
	jsr	pc,expres
	jsr	pc,checkreg
	jsr	pc,checkrp
	add	$2,dot
	clr	r0
	rts	pc
2:
	cmp	r3,$24		/ register type
	bne	1f
	jsr	pc,checkreg
	clr	r0
	rts	pc
1:
	add	$2,dot
	clr	r0
	rts	pc

alp:
	jsr	pc,readop
	jsr	pc,expres
	jsr	pc,checkrp
	jsr	pc,checkreg
	cmp	r4,$'+
	bne	1f
	jsr	pc,readop
	clr	r0
	rts	pc
1:
	mov	$2,r0
	rts	pc

amin:
	jsr	pc,readop
	cmp	r4,$'(
	beq	1f
	mov	r4,savop
	mov	$'-,r4
	br	getx
1:
	jsr	pc,readop
	jsr	pc,expres
	jsr	pc,checkrp
	jsr	pc,checkreg
	clr	r0
	rts	pc

adoll:
	jsr	pc,readop
	jsr	pc,expres
	add	$2,dot
	clr	r0
	rts	pc

astar:
	jsr	pc,readop
	cmp	r4,$'*
	bne	1f
	jsr	r5,error; '*
1:
	jsr	pc,addres
	add	r0,dot
	rts	pc

errora:
	jsr	r5,error; 'a
	rts	pc

checkreg:
	cmp	r2,$7
	bhi	1f
	cmp	r3,$1
	beq	2f
	cmp	r3,$4
	bhi	2f
1:
	jsr	pc,errora
2:
	rts	pc

errore:
	jsr	r5,error; 'e
	rts	pc

checkrp:
	cmp	r4,$')
	beq	1f
	jsr	r5,error; ')
	rts	pc
1:
	jsr	pc,readop
	rts	pc

