/ C library-- floating output

.globl	pfloat
.globl	pscien
.globl	pgen
.globl	fltused

.globl	_ecvt
.globl	_fcvt
.globl	_gcvt

fltused:		/ force loading

pgen:
	mov	r3,-(sp)
	mov	r0,-(sp)
	tst	r2
	bne	1f
	mov	$6,(sp)
1:
	movf	(r4)+,fr0
	movf	fr0,-(sp)
	jsr	pc,_gcvt
	add	$8+2+2,sp
1:
	tstb	(r3)+
	bne	1b
	dec	r3
	rts	pc

pfloat:
	mov	$sign,-(sp)
	mov	$decpt,-(sp)
	tst	r2
	bne	1f
	mov	$6,r0
1:
	mov	r0,-(sp)
	mov	r0,ndigit
	movf	(r4)+,fr0
	movf	fr0,-(sp)
	jsr	pc,_fcvt
	add	$8+2+2+2,sp
	tst	sign
	beq	1f
	movb	$'-,(r3)+
1:
	mov	decpt,r2
	bgt	1f
	movb	$'0,(r3)+
1:
	mov	r2,r1
	ble	1f
2:
	movb	(r0)+,(r3)+
	sob	r1,2b
1:
	mov	ndigit,r1
	beq	1f
	movb	$'.,(r3)+
1:
	neg	r2
	ble	1f
2:
	dec	r1
	blt	1f
	movb	$'0,(r3)+
	sob	r2,2b
1:
	tst	r1
	ble	2f
1:
	movb	(r0)+,(r3)+
	sob	r1,1b
2:
	rts	pc

pscien:
	mov	$sign,-(sp)
	mov	$decpt,-(sp)
	mov	r0,-(sp)
	mov	r0,ndigit
	tst	r2
	bne	1f
	mov	$6,(sp)
1:
	movf	(r4)+,fr0
	movf	fr0,-(sp)
	jsr	pc,_ecvt
	add	$8+2+2+2,sp
	tst	sign
	beq	1f
	movb	$'-,(r3)+
1:
	cmpb	(r0),$'0
	bne	1f
	inc	decpt
1:
	movb	(r0)+,(r3)+
	movb	$'.,(r3)+
	mov	ndigit,r1
	dec	r1
	ble	1f
2:
	movb	(r0)+,(r3)+
	sob	r1,2b
1:
	movb	$'e,(r3)+
	mov	decpt,r2
	dec	r2
	mov	r2,r1
	bge	1f
	movb	$'-,(r3)+
	neg	r1
	br	2f
1:
	movb	$'+,(r3)+
2:
	clr	r0
	div	$10.,r0
	add	$'0,r0
	movb	r0,(r3)+
	add	$'0,r1
	movb	r1,(r3)+
	rts	pc
.data
sign:	.=.+2
ndigit:	.=.+2
decpt:	.=.+2
