/ C library-- floating output

.globl	pfloat
.globl	pscien
.globl	fltused

.globl	_ndigit
.globl	ecvt
.globl	fcvt

fltused:		/ force loading
pfloat:
	mov	r0,_ndigit
	tst	r2
	bne	1f
	mov	$6,_ndigit
1:
	movf	(r4)+,fr0
	jsr	pc,fcvt
	tst	r1
	beq	1f
	movb	$'-,(r3)+
1:
	tst	r2
	bgt	1f
	movb	$'0,(r3)+
1:
	mov	r2,r1
	ble	1f
2:
	movb	(r0)+,(r3)+
	sob	r1,2b
1:
	mov	_ndigit,r1
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
	mov	r0,_ndigit
	tst	r2
	bne	1f
	mov	$6,_ndigit
1:
	movf	(r4)+,fr0
	jsr	pc,ecvt
	tst	r1
	beq	1f
	movb	$'-,(r3)+
1:
	movb	(r0)+,(r3)+
	movb	$'.,(r3)+
	mov	_ndigit,r1
	dec	r1
	ble	1f
2:
	movb	(r0)+,(r3)+
	sob	r1,2b
1:
	movb	$'e,(r3)+
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
