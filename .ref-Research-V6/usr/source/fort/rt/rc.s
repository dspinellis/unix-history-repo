/
/

/ rc -- complex arithmetic

.globl	c8c16
.globl	c16c8
.globl	i4c8
.globl	r4c8
.globl	r8c8
.globl	i4c16
.globl	r4c16
.globl	r8c16
.globl	cad8
.globl	csb8
.globl	cmp8
.globl	cdv8
.globl	cng8
.globl	cad16
.globl	csb16
.globl	cmp16
.globl	cdv16
.globl	cng16
.globl	rval16
.globl	rval16p
.globl	gas16
.globl	ceq8
.globl	cne8
.globl	ceq16
.globl	cne16

.globl	cpi8
.globl	cpi16

one = 040200
a = r2
b = r3
c = r4
d = r5

e = r0
f = r1

gas16:
	mov	16.(sp),r0
	setd
	movf	(sp)+,a
	movf	(sp)+,b
	tst	(sp)+
	movf	a,(r0)+
	movf	b,(r0)+
	jmp	*(r4)+

rval16p:
	mov	r3,r0
	add	(r4)+,r0
	mov	(r0),r0
	br	1f

rval16:
	mov	(r4)+,r0
1:
	setd
	movf	(r0)+,a
	movf	(r0)+,b
	movf	b,-(sp)
	movf	a,-(sp)
	jmp	*(r4)+

cad16:
	setd
	br	1f

cad8:
	setf
1:
	jsr	pc,garg
	addf	a,e
	addf	b,f
	br	sarg

csb16:
	setd
	br	1f

csb8:
	setf
1:
	jsr	pc,garg
	subf	a,e
	negf	e
	subf	b,f
	negf	f
	br	sarg

cmp16:
	setd
	br	1f

cmp8:
	setf
1:
	jsr	pc,garg
	mulf	a,e
	mulf	b,f
	subf	f,e
	mulf	d,a
	mulf	c,b
	movf	a,f
	addf	b,f

sarg:
	movf	f,-(sp)
	movf	e,-(sp)
	jmp	*(r4)+

cdv16:
	setd
	br	1f

cdv8:
	setf
1:
divide:
	jsr	pc,garg
	absf	e
	absf	f
	cmpf	e,f
	cfcc
	blt	1f

/ algorithm #1 |c| > |d|

	movf	d,e
	divf	c,e		/ r = d/c
	movf	d,f
	mulf	e,f
	addf	c,f		/ x = c+rd
	movf	f,c
	movf	a,f
	mulf	e,f
	negf	f
	addf	b,f
	mulf	b,e
	addf	a,e
	divf	c,e
	divf	c,f
	br	sarg

/ algorithm #2 |c| < |d|

1:
	movf	c,e
	divf	d,e		/ r = c/d
	movf	c,f
	mulf	e,f
	addf	d,f		/ x = d+rc
	movf	f,c
	movf	b,f
	mulf	e,f
	subf	a,f
	mulf	a,e
	addf	b,e
	divf	c,e
	divf	c,f
	br	sarg

cng16:
	setd
	br	1f

cng8:
	setf
1:
	movf	(sp)+,a
	negf	a
	negf	(sp)
	movf	a,-(sp)
	jmp	*(r4)+

/ setup the following registers
/ (a,bi) +o (c,di) -> (e+fi)
/ a,b,c,d are input
/ e,f are output and also contain
/ c,d on input

garg:
	mov	(sp)+,r0
	movf	(sp)+,e
	movf	e,c
	movf	(sp)+,f
	movf	f,d
	movf	(sp)+,a
	movf	(sp)+,b
	jmp	(r0)

i4c8:
	setf
	br	1f

i4c16:
	setd
1:
	setl
	movif	(sp)+,fr0
	br	2f

r4c8:
	setf
	br	1f

r8c16:
	setd
1:
	movf	(sp)+,fr0
	br	2f

r4c16:
	setd
	br	1f

r8c8:
	setf
1:
	movof	(sp)+,fr0
2:
	clrf	-(sp)
	movf	fr0,-(sp)
	jmp	*(r4)+

c16c8:
	setd
	br	1f

c8c16:
	setf
1:
	movf	(sp)+,r0
	movf	(sp)+,r1
	movfo	r1,-(sp)
	movfo	r0,-(sp)
	jmp	*(r4)+

ceq16:
	setd
	br	1f
ceq8:
	setf
1:
	mov	$1,r1
	br	2f

cne16:
	setd
	br	1f

cne8:
	setf
1:
	clr	r1
2:
	jsr	pc,garg
	cmpf	c,a
	cfcc
	bne	1f
	cmpf	d,b
	cfcc
	beq	2f
1:
	inc	r1
	bic	$2,r1
2:
	mov	r1,-(sp)
	jmp	*(r4)+

cpi8:
	setf
	br	1f

cpi16:
	setd
1:
	clr	r0
	tst	(sp)+
	mov	(sp)+,r1
	bge	1f
	inc	r0
	neg	r1
1:
	movf	(sp)+,fr0
	movf	fr0,fr4
	movf	(sp)+,fr0
	movf	fr0,fr5
	movf	$one,fr0
	clrf	fr1
1:
	dec	r1
	bmi	1f
	movf	fr0,fr2
	movf	fr1,fr3
	mulf	fr4,fr0
	mulf	fr4,fr1
	mulf	fr5,fr2
	mulf	fr5,fr3
	subf	fr3,fr0
	addf	fr2,fr1
	br	1b
1:
	tst	r0
	beq	1f
	clrf	-(sp)
	movf	$one,fr2
	movf	fr2,-(sp)
	movf	fr1,-(sp)
	movf	fr0,-(sp)
	jmp	divide
1:
	movf	fr1,-(sp)
	movf	fr0,-(sp)
	jmp	*(r4)+
