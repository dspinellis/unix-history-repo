ldfps = 170100^tst
stfps = 170200^tst
/ ftoa -- basic g fp conversion

.globl	_ndigit
.globl ecvt
.globl fcvt


/ ecvt converts fr0 into decimal
/ the string of converted digits is pointed to by r0.
/ the number of digits are specified by _ndigit
/ r2 contains the decimal point
/ r1 contains the sign

.globl	_ecvt, _fcvt

_ecvt:
	mov	r5,-(sp)
	mov	sp,r5
	mov	r2,-(sp)
	movf	4(r5),fr0
	mov	14(r5),_ndigit
	jsr	pc,ecvt
	br	1f

_fcvt:
	mov	r5,-(sp)
	mov	sp,r5
	mov	r2,-(sp)
	movf	4(r5),fr0
	mov	14(r5),_ndigit
	jsr	pc,fcvt
1:
	mov	r2,*16(r5)
	mov	r1,*20(r5)
	mov	(sp)+,r2
	mov	(sp)+,r5
	rts	pc

fcvt:
	clr	eflag
	br	1f
ecvt:
	mov	$1,eflag
1:
	stfps	-(sp)
	ldfps	$200
	movf	fr0,-(sp)
	movf	fr1,-(sp)
	mov	r3,-(sp)
	mov	$buf,r1
	clr	r2
	clr	sign
	tstf	fr0
	cfcc
	beq	zer
	bgt	1f
	inc	sign
	negf	fr0
1:
	modf	$one,fr0
	tstf	fr1
	cfcc
	beq	lss

gtr:
	movf	fr0,-(sp)
	movf	fr1,fr0
1:
	mov	$buftop,r3
1:
	modf	tenth,fr0
	movf	fr0,fr2
	movf	fr1,fr0
	addf	$epsilon,fr2
	modf	$ten,fr2
	movfi	fr3,r0
	add	$'0,r0
	movb	r0,-(r3)
	inc	r2
	tstf	fr0
	cfcc
	bne	1b
/
	mov	$buf,r1
1:
	movb	(r3)+,(r1)+
	cmp	r3,$buftop
	blo	1b
/
	movf	(sp)+,fr0
	br	pad

zer:
	inc	r2
	br	pad

lss:
	dec	r2
	modf	$ten,fr0
	tstf	fr1
	cfcc
	beq	lss
	inc	r2
	jsr	pc,digit1

pad:
	jsr	pc,digit
		br out
	br	pad

digit:
	cmp	r1,$buftop
	bhis	1f
	add	$2,(sp)
	modf	$ten,fr0

digit1:
	movfi	fr1,r0
	add	$'0,r0
	movb	r0,(r1)+
1:
	rts	pc
/
out:
	mov	$buf,r0
	add	_ndigit,r0
	tst	eflag
	bne	1f
	add	r2,r0
1:
	cmp	r0,$buf
	blo	outout
	movb	(r0),r3
	add	$5,r3
	movb	r3,(r0)
1:
	cmpb	(r0),$'9
	ble	1f
	movb	$'0,(r0)
	cmp	r0,$buf
	blos	2f
	incb	-(r0)
	br	1b
2:
	movb	$'1,(r0)
	inc	r2
1:
outout:
	mov	sign,r1
	mov	_ndigit,r0
	tst	eflag
	bne	1f
	add	r2,r0
1:
	clrb	buf(r0)
	mov	$buf,r0
	mov	(sp)+,r3
	movf	(sp)+,fr1
	movf	(sp)+,fr0
	ldfps	(sp)+
	rts	pc

epsilon = 037114
one	= 40200
ten	= 41040
	.data
tenth:	037314; 146314; 146314; 146315
_ndigit:10.
	.bss
buf:	.=.+40.
buftop:
sign:	.=.+2
eflag:	.=.+2
	.text
