/
/ copyright 1972 bell telephone laboratories inc.
/

/ bas2 -- expression evaluation

expr:
	jsr	pc,e1
	jsr	pc,rval
	rts	pc

/ assignment right to left
e1:
	jsr	pc,e2
	cmp	r0,$'=
	beq	1f
	jsr	pc,rval
	rts	pc
1:
	tst	val
	beq	1f
	jsr	pc,serror
1:
	jsr	pc,e1
	jsr	r5,op; _asgn
	rts	pc

/ and or left to right
e2:
	jsr	pc,e3
1:
	cmp	r0,$'&
	beq	2f
	cmp	r0,$'|
	beq	3f
	rts	pc
2:
	jsr	pc,rval
	jsr	pc,e3
	jsr	r5,op; _and
	br	1b
3:
	jsr	pc,rval
	jsr	pc,e3
	jsr	r5,op; _or
	br	1b

/ relation extended relation
e3:
	jsr	pc,e4
	jsr	pc,e3a
		rts pc
	clr	-(sp)
1:
	mov	r0,-(sp)
	jsr	pc,e4
	jsr	pc,rval
	mov	(sp)+,(r4)+
	jsr	pc,e3a
		br 1f
	mov	$_extr,(r4)+
	inc	(sp)
	br	1b
1:
	dec	(sp)
	blt	1f
	mov	$_and,(r4)+
	br	1b
1:
	tst	(sp)+
	rts	pc

/ relational operator
e3a:
	cmp	r0,$'>
	beq	1f
	cmp	r0,$'<
	beq	2f
	cmp	r0,$'=
	beq	3f
	rts	pc
1:
	mov	$_great,r0
	cmpb	(r3),$'=
	bne	1f
	inc	r3
	mov	$_greateq,r0
	br	1f
2:
	cmpb	(r3),$'>
	bne	2f
	inc	r3
	mov	$_noteq,r0
	br	1f
2:
	mov	$_less,r0
	cmpb	(r3),$'=
	bne	1f
	inc	r3
	mov	$_lesseq,r0
	br	1f
3:
	cmpb	(r3),$'=
	beq	2f
	rts	pc
2:
	inc	r3
	mov	$_equal,r0
1:
	jsr	pc,rval
	add	$2,(sp)
	rts	pc

/ add subtract
e4:
	jsr	pc,e5
1:
	cmp	r0,$'+
	beq	2f
	cmp	r0,$'-
	beq	3f
	rts	pc
2:
	jsr	pc,rval
	jsr	pc,e5
	jsr	r5,op; _add
	br	1b
3:
	jsr	pc,rval
	jsr	pc,e5
	jsr	r5,op; _sub
	br	1b

/ multiply divide
e5:
	jsr	pc,e6
1:
	cmp	r0,$'*
	beq	2f
	cmp	r0,$'/
	beq	3f
	rts	pc
2:
	jsr	pc,rval
	jsr	pc,e6
	jsr	r5,op; _mult
	br	1b
3:
	jsr	pc,rval
	jsr	pc,e6
	jsr	r5,op; _divid
	br	1b

/ exponential
e6:
	jsr	pc,e7
1:
	cmp	r0,$'^
	beq	2f
	rts	pc
2:
	jsr	pc,rval
	jsr	pc,e7
	jsr	r5,op; _expon
	br	1b

/ primary
e7:
	movb	(r3)+,r0
	jsr	pc,skip
	cmp	r0,$'_
	bne	1f
	jsr	pc,e7
	mov	$_negat,(r4)+
	rts	pc
1:
	mov	$1,val
	cmp	r0,$'(
	bne	1f
	jsr	pc,e1
	cmp	r0,$')
	bne	2f
	movb	(r3)+,r0
	br	e7a
2:
	jsr	pc,serror
1:
	cmp	r0,$'.
	beq	2f
	jsr	pc,digit
		br 1f
2:
	dec	r3
	jsr	r5,atof; nextc
	jsr	pc,const
	br	e7a
1:
	jsr	pc,alpha
		br jim
	jsr	pc,name
		br 2f
	jsr	r5,error; <reserved name\n\0>; .even
2:
	mov	$_lval,(r4)+
	mov	r1,(r4)+
	clr	val
	br	e7a
jim:
	jsr	pc,serror

e7a:
	jsr	pc,skip
	cmp	r0,$'(
	bne	1f
	jsr	pc,rval
	jsr	r5,rlist; _funct
	cmp	r0,$')
	bne	jim
	movb	(r3)+,r0
	br	e7a
1:
	cmp	r0,$'[
	bne	1f
	tst	val
	beq	2f
	jsr	pc,serror
2:
	jsr	r5,rlist; _subscr
	clr	val
	cmp	r0,$']
	bne	jim
	movb	(r3)+,r0
	br	e7a
1:
	rts	pc

op:
	jsr	pc,rval
	mov	(r5)+,(r4)+
	rts	r5

rval:
	tst	val
	bne	1f
	mov	$_rval,(r4)+
	inc	val
1:
	rts	pc

const:
	mov	$_const,(r4)+
	movf	r0,(r4)+
	rts	pc

rlist:
	clr	-(sp)
	cmpb	(r3),$')
	bne	1f
	movb	(r3)+,r0
	br	2f
1:
	inc	(sp)
	jsr	pc,expr
	cmp	r0,$',
	beq	1b
2:
	mov	(r5)+,(r4)+
	mov	(sp)+,(r4)+
	rts	r5

