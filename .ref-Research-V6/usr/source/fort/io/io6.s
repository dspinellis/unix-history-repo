/
/

/ io6 --  input conversions

/.globl	aicv
/.globl	gicv
/
/.globl	ilval
/.globl	width
/.globl	ilen
/.globl	fgetc
/.globl	itype
/.globl	nlflg
/.globl	gcflg

aicv:
	mov	ilval,r1
	movb	width,r2
	movb	ilen,r0
	mov	r0,-(sp)
1:
	cmp	r2,(sp)
	ble	1f
	jsr	r5,fgetc
	dec	r2
	br	1b
1:
	tst	r2
	ble	1f
	jsr	r5,fgetc
	movb	r0,(r1)+
	dec	r2
	dec	(sp)
	br	1b
1:
	tst	(sp)
	ble	1f
	movb	$' ,(r1)+
	dec	(sp)
	br	1b
1:
	tst	(sp)+
	rts	r5

licv:
	mov	width,twidth
	setd
	seti
	clrf	fr0
1:
	jsr	r5,fgetcn
	cmp	r0,$'t
	beq	2f
	cmp	r0,$'T
	beq	2f
	cmp	r0,$'1
	beq	2f
	cmp	r0,$',
	beq	1f
	br	1b
2:
	movif	$1,fr0
	br	1b
1:
	br	storin

iicv:
	clr	ndig

ficv:
eicv:
dicv:
	mov	width,twidth
	br	1f

gicv:
	mov	$16383.,twidth
	clr	ndig
	mov	pc,gcflg
	br	2f
1:
	clr	gcflg
2:
	jsr	r5,gatof
storin:
	cmpb	itype,$'r
	beq	1f
	cmpb	ilen,$1
	beq	3f
	cmpb	ilen,$4
	bne	2f
	setl
2:
	movfi	fr0,*ilval
	rts	r5
3:
	movfi	fr0,r0
	movb	r0,*ilval
	rts	r5
1:
	cmpb	ilen,$8.
	beq	2f
	setf
2:
	movf	fr0,*ilval
	rts	r5

gatof:
	setd
	seti
	movif	$10.,fr3
	clr	r2
	clrf	fr0
	clr	-(sp)
1:
	jsr	r5,fgetcn
	cmp	$' ,r0
	bne	1f
	tst	nlflg
	beq	1b
	tst	(sp)+
	rts	r5
1:
	cmp	r0,$'+
	beq	1f
	cmp	r0,$'-
	bne	2f
	inc	(sp)
1:
	jsr	r5,fgetcn
2:
	cmp	$' ,r0
	bne	3f
	tst	gcflg
	bne	3f
	mov	$'0,r0
3:
	sub	$'0,r0
	cmp	r0,$9.
	bhi	2f
	mulf	fr3,fr0
	movif	r0,fr1
	addf	fr1,fr0
	dec	r1
	br	1b
2:
	add	$'0,r0
	cmp	r0,$'.
	bne	1f
	inc	r2
	clr	r1
	br	1b
1:
	mov	r3,-(sp)
	clr	r3
	cmp	r0,$'d
	beq	3f
	cmp	r0,$'+
	beq	3f
	cmp	r0,$'-
	beq	3f
	cmp	r0,$'e
	bne	2f
3:
	jsr	r5,atoi
2:
	tst	r2
	bne	1f
	mov	ndig,r1
	neg	r1
1:
	movf	fr3,fr2
	add	r3,r1
	mov	(sp)+,r3
	tst	r1
	beq	1f
	bpl	3f
	neg	r1
	mov	pc,-(sp)
	br	2f
3:
	clr	-(sp)
2:
	dec	r1
	ble	2f
	mulf	fr3,fr2
	br	2b
2:
	tst	(sp)+
	bne	2f
	mulf	fr2,fr0
	br	1f
2:
	divf	fr2,fr0
1:
	tst	(sp)+
	beq	1f
	negf	r0
1:
	cmp	r0,$',
	beq	1f
	cmp	$' ,r0
	beq	1f
	jsr	r5,rerr; 110.
1:
	rts	r5

atoi:
	clr	-(sp)
	cmp	r0,$'+
	beq	1f
	cmp	r0,$'-
	beq	3f
	jsr	r5,fgetcn
	cmp	r0,$'+
	beq	1f
	cmp	r0,$'-
	bne	2f
3:
	inc	(sp)
1:
	jsr	r5,fgetcn
2:
	sub	$'0,r0
	cmp	r0,$'9.
	bhi	2f
	mpy	$10.,r3
	add	r0,r3
	br	1b
2:
	add	$'0,r0
	tst	(sp)+
	beq	1f
	neg	r3
1:
	rts	r5

fgetcn:
	tst	twidth
	bgt	1f
	mov	$',,r0
	rts	r5
1:
	jsr	r5,fgetc
	dec	twidth
	rts	r5

