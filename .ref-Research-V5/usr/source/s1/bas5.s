/ bas4 -- old library routines
atoi:
	clr	r1
	jsr	r5,*(r5)
	clr	-(sp)
	cmp	r0,$'-
	bne	2f
	inc	(sp)
1:
	jsr	r5,*(r5)
2:
	sub	$'0,r0
	cmp	r0,$9
	bhi	1f
	mpy	$10.,r1
	add	r0,r1
	br	1b
1:
	add	$'0,r0
	tst	(sp)+
	beq	1f
	neg	r1
1:
	tst	(r5)+
	rts	r5

ldfps = 170100^tst
stfps = 170200^tst
atof:
	stfps	-(sp)
	ldfps	$200
	movf	fr1,-(sp)
	mov	r1,-(sp)
	mov	r2,-(sp)
	clr	-(sp)
	clrf	fr0
	clr	r2
	jsr	r5,*(r5)
	cmpb	r0,$'-
	bne	2f
	inc	(sp)
1:
	jsr	r5,*(r5)
2:
	sub	$'0,r0
	cmp	r0,$9.
	bhi	2f
	jsr	pc,dig
		br	1b
	inc	r2
	br	1b
2:
	cmpb	r0,$'.-'0
	bne	2f
1:
	jsr	r5,*(r5)
	sub	$'0,r0
	cmp	r0,$9.
	bhi	2f
	jsr	pc,dig
		dec r2
	br	1b
2:
	cmpb	r0,$'e-'0
	bne	1f
	mov	(r5),0f
	jsr	pc,9f
.data
9:
	jsr	r5,atoi; 0:..
	rts	pc
.text
	sub	$'0,r0
	add	r1,r2
1:
	movf	$one,fr1
	mov	r2,-(sp)
	beq	2f
	bgt	1f
	neg	r2
1:
	cmp	r2,$38.
	blos	1f
	clrf	fr0
	tst	(sp)+
	bmi	out
	movf	$huge,fr0
	br	out
1:
	mulf	$ten,fr1
	sob	r2,1b
2:
	tst	(sp)+
	bge	1f
	divf	fr1,fr0
	br	2f
1:
	mulf	fr1,fr0
	cfcc
	bvc	2f
	movf	$huge,fr0
2:
out:
	tst	(sp)+
	beq	1f
	negf	fr0
1:
	add	$'0,r0
	mov	(sp)+,r2
	mov	(sp)+,r1
	movf	(sp)+,fr1
	ldfps	(sp)+
	tst	(r5)+
	rts	r5

dig:
	cmpf	$big,fr0
	cfcc
	blt	1f
	mulf	$ten,fr0
	movif	r0,fr1
	addf	fr1,fr0
	rts	pc
1:
	add	$2,(sp)
	rts	pc

one	= 40200
ten	= 41040
big	= 56200
huge	= 77777

.globl	_ndigit
.globl ecvt
.globl fcvt

ftoa:
	jsr	pc,ecvt
	mov	r0,bufptr
	tstb	r1
	beq	1f
	mov	$'-,r0
	jsr	r5,*(r5)
1:
	cmp	r2,$-2
	blt	econ
	cmp	r2,$5
	bgt	econ
	jsr	pc,cout
	tst	(r5)+
	rts	r5

econ:
	mov	r2,-(sp)
	mov	$1,r2
	jsr	pc,cout
	mov	$'e,r0
	jsr	r5,*(r5)
	mov	(sp)+,r0
	dec	r0
	jmp	itoa

cout:
	mov	bufptr,r1
	add	_ndigit,r1
	mov	r2,-(sp)
	add	bufptr,r2
1:
	cmp	r1,r2
	blos	1f
	cmpb	-(r1),$'0
	beq	1b
	inc	r1
1:
	mov	(sp)+,r2
	bge	2f
	mov	$'.,r0
	jsr	r5,*(r5)
1:
	mov	$'0,r0
	jsr	r5,*(r5)
	inc	r2
	blt	1b
	dec	r2
2:
	mov	r2,-(sp)
	mov	bufptr,r2
1:
	cmp	r2,r1
	bhis	1f
	tst	(sp)
	bne	2f
	mov	$'.,r0
	jsr	r5,*(r5)
2:
	dec	(sp)
	movb	(r2)+,r0
	jsr	r5,*(r5)
	br	1b
1:
	tst	(sp)+
	rts	pc

.bss
bufptr:	.=.+2
.text

ftoo:
	stfps	-(sp)
	ldfps	$200
	mov	r1,-(sp)
	mov	r2,-(sp)
	mov	$buf,r1
	movf	fr0,(r1)+
	mov	$buf,r2
	br	2f
1:
	cmp	r2,r1
	bhis	1f
	mov	$';,r0
	jsr	r5,*(r5)
2:
	mov	(r2)+,r0
	jsr	pc,oct
	br	1b
1:
	mov	$'\n,r0
	jsr	pc,*(r5)+
	ldfps	(sp)+
	rts	r5

oct:
	mov	r0,x+2
	setl
	movif	x,fr0
	mulf	$small,fr0
	seti
	mov	$6.,-(sp)
1:
	modf	$eight,fr0
	movfi	fr1,r0
	add	$'0,r0
	jsr	r5,*(r5)
	dec	(sp)
	bne	1b
	tst	(sp)+
	rts	pc

eight	= 41000
small	= 33600
.bss
buf:	.=.+8
x:	.=.+4
.text

itoa:
	mov	r1,-(sp)
	mov	r0,r1
	bge	1f
	neg	r1
	mov	$'-,r0
	jsr	r5,*(r5)
1:
	jsr	pc,1f
	mov	(sp)+,r1
	tst	(r5)+
	rts	r5

1:
	clr	r0
	dvd	$10.,r0
	mov	r1,-(sp)
	mov	r0,r1
	beq	1f
	jsr	pc,1b
1:
	mov	(sp)+,r0
	add	$'0,r0
	jsr	r5,*(r5)
	rts	pc
