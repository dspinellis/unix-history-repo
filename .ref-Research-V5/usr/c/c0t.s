/ assure fake printf (no floating)

.globl	fltused; fltused = 0

/ convert stream to number; result is type.
/ value in cval or fcval

fpp = 1

.globl	_getnum

.globl	_peekc
.globl	_getchar
.globl	_cval
.globl	_fcval
.globl	_error

_getnum:
	.if	fpp
	movif	$10.,fr3
	clrf	fr0
	.endif
	clr	nfract
	clr	totdig
	clr	decpt
	clr	_cval
	mov	2(sp),base
	mov	r2,-(sp)
1:
	jsr	r5,getdig
		br 2f
	.if	fpp
	mulf	fr3,fr0
	movif	r0,fr1
	addf	fr1,fr0
	.endif
	inc	nfract
	br	1b
2:
	tst	decpt
	bne	1f
	clr	nfract
	cmp	r0,$'.
	bne	1f
	mov	pc,decpt
	br	1b
1:
	tst	totdig
	beq	1f
	cmp	r0,$'e
	bne	1f
	clr	-(sp)
	clr	_cval
	mov	pc,decpt
	clr	_cval
	mov	$10.,base
	jsr	pc,_getchar
	cmp	r0,$'+
	beq	2f
	cmp	r0,$'-
	bne	3f
	inc	(sp)
	br	2f
3:
	mov	r0,_peekc
2:
	jsr	r5,getdig
		br 2f
	br	2b
2:
	tst	(sp)+
	beq	2f
	neg	_cval
2:
	sub	_cval,nfract
1:
	mov	r0,_peekc
	tst	totdig
	bne	1f
	mov	$39.,r0		/ "." operator
9:
	mov	(sp)+,r2
	rts	pc
1:
	tst	decpt
	bne	1f
	mov	$21.,r0		/ fixed constant
	br	9b
1:
	.if	fpp
	movif	$1,fr2
	mov	nfract,r2
	mov	r2,-(sp)
	beq	2f
	bgt	1f
	neg	r2
1:
	mulf	fr3,fr2
	sob	r2,1b
2:
	tst	(sp)+
	ble	1f
	divf	fr2,fr0
	br	2f
1:
	mulf	fr2,fr0
2:
	mov	$_fcval,r0
	movf	fr0,(r0)
	tst	(r0)+
	tst	(r0)+
	bne	1f
	tst	(r0)+
	bne	1f
	tst	(r0)+
	bne	1f
	mov	$24.,r0
	mov	_fcval,_cval
	br	9b
1:
	mov	$23.,r0
	br	9b
	.endif
	.if	1-fpp
	mov	$fperr,-(sp)
	jsr	pc,_error
	tst	(sp)+
	mov	$21.,r0
	br	9b
fperr:	<No floating point!\0>; .even
	.endif

getdig:
	mov	_peekc,r0
	beq	1f
	clr	_peekc
	br	2f
1:
	jsr	pc,_getchar
2:
	sub	$'0,r0
	cmp	r0,$9.
	bhi	1f
	inc	totdig
	mov	_cval,r1
	mul	base,r1
	add	r0,r1
	mov	r1,_cval
	tst	(r5)+
	rts	r5
1:
	add	$'0,r0
	rts	r5

.bss
base:	.=.+2
nfract:	.=.+2
decpt:	.=.+2
totdig:	.=.+2
