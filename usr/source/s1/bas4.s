/
/ copyright 1972 bell telephone laboratories inc.
/

/ bas4 -- builtin functions

builtin:
	dec	sublev
	mov	(r3)+,sstack
	mov	(r3)+,r4
	movfi	r0,r0
	com	r0
	asl	r0
	cmp	r0,$2f-1f
	bhis	2f
	jmp	*1f(r0)
1:
	fnarg
	fnexp
	fnlog
	fnsin
	fncos
	fnatan
	fnrand
	fnexpr
	fnint
	fnabs
	fnsqr
2:
	mov	$-1,r0
	jsr	pc,getloc		/ label not found diagnostic

fnarg:
	cmp	(r4)+,$1
	bne	narg
	movf	(r3),r0
	movfi	r0,r0
	jsr	pc,arg
	br	fnadvanc

fnexp:
	jsr	r5,fnfn; exp
	br	fnadvanc

fnlog:
	jsr	r5,fnfn; log
	bec	fnadvanc
	jsr	r5,error
		<Bad log\n\0>; .even

fnsin:
	jsr	r5,fnfn; sin
	bec	fnadvanc
	jsr	r5,error
		<Bad sine\n\0>; .even

fncos:
	jsr	r5,fnfn; cos
	bec	fnadvanc
	jsr	r5,error
		<Bad cosine\n\0>; .even

fnatan:
	jsr	r5,fnfn; atan
	bec	fnadvanc
	jsr	r5,error
		<Bad arctangent\n\0>; .even

fnrand:
	tst	(r4)+
	bne	narg
	jsr	pc,rand
	movif	r0,r0
	divf	$44000,r0
	jmp	advanc

fnexpr:
	tst	(r4)+
	bne	narg
	mov	r3,-(sp)
	mov	r4,-(sp)
	jsr	pc,rdline
	mov	exprloc,r4
	mov	$line,r3
	jsr	pc,expr
	mov	$_tra,(r4)+
	mov	(sp)+,(r4)+
	mov	(sp)+,r3
	mov	exprloc,r4
	add	$8,r3
	jmp	*(r4)+

fnint:
	cmp	(r4)+,$1
	bne	narg
	movf	(r3),r0
	modf	$one,r0
	movf	r1,r0
	br	fnadvanc

fnabs:
	cmp	(r4)+,$1
	bne	narg
	movf	(r3),r0
	cfcc
	bge	fnadvanc
	negf	r0
	br	fnadvanc

fnsqr:
	jsr	r5,fnfn; sqrt
	bec	fnadvanc
	jsr	r5,error
		<Bad square root\n\0>; .even

fnadvanc:
	add	$8,r3
	jmp	advanc

narg:
	jsr	r5,error
		<arg count\n\0>; .even

arg:
	tst	sublev
	beq	1f
	mov	sstack,r1
	sub	*2(r1),r0
	bhi	1f
2:
	inc	r0
	bgt	2f
	add	$8,r1
	br	2b
2:
	movf	4(r1),r0
	rts	pc
1:
	jsr	r5,error
		<bad arg\n\0>; .even

fnfn:
	cmp	(r4)+,$1
	bne	narg
	movf	(r3),r0
	mov	(r5)+,pc

draw:
	tstf	r2
	cfcc
	bne	1f
	movf	r0,drx
	movf	r1,dry
	rts	r5
1:
	movf	r0,-(sp)
	movf	r1,-(sp)
	mov	$3,r0
	jsr	pc,drput
	jsr	pc,drxy
	movf	(sp)+,r0
	movf	r0,dry
	movf	(sp)+,r0
	movf	r0,drx
	jsr	pc,drxy
	rts	r5

drxy:
	movf	drx,r0
	jsr	pc,drco
	movf	dry,r0

drco:
	tstf	r0
	cfcc
	bge	1f
	clrf	r0
1:
	cmpf	$40200,r0		/ 1.0
	cfcc
	bgt	1f
	movf	$40177,r0		/ 1.0-eps
1:
	subf	$40000,r0		/ .5
	mulf	$43200,r0		/ 4096
	movfi	r0,r0
	mov	r0,-(sp)
	jsr	pc,drput
	mov	(sp)+,r0
	swab	r0

drput:
	movb	r0,ch
	mov	drfo,r0
	bne	1f
	sys	open; vt; 1
	bec	2f
	4
2:
	mov	r0,drfo
1:
	sys	write; ch; 1
	rts	pc

