.globl _preadn
.text
/
/ preadn(pfloat, pdigits, pbuf)
/ char *pbuf;
/
/ This routine reads a number from the current active file.
/ pdigits is the size of pbuf in characters.
/ pfloat is 0 to read an integer, 1 to read a real number.
/ it returns with the carry bit set if it reaches EOF before
/ reading any characters of the number while EOF was not true
/ before the call.
/
pfloat = 2.
pdigits = 4.
pbuf = 6.
/
_preadn:
	mov	pbuf(sp),r2
	mov	pdigits(sp),r3
	jsr	pc,iosync
	mov	buf,r0
	mov	FUNIT(r0),flags
	bit	$EOF,flags
	bne	9f
	br	2f
/
/ skip white space
/
1:
	jsr	pc,get
	jsr	pc,iosync
	bit	$EOF,FUNIT(r0)
	bne	prsec
2:
	movb	*buf,r0
	cmp	r0,$' 
	beq	1b
	cmp	r0,$'\t
	beq	1b
	cmp	r0,$14
	beq	1b
	cmp	r0,$'\n
	beq	1b
/
/ starting point for
/ a signed number
/ record + and - if present
/
psignd:
	clr	r1		/ digit count
	cmp	r0,$'-
	bne	1f
	dec	r3
	ble	prtood
	movb	r0,(r2)+
	br	2f
1:
	cmp	r0,$'+
	bne	1f
2:
	jsr	pc,get
	br	1f
/
/ starting point for
/ a unsigned number
/
punsd:
	clr	r1
1:
	jsr	pc,iosync
	bit	$EOF,flags
	bne	2f
	movb	*buf,r0
	cmp	r0,$'0
	blt	2f
	cmp	r0,$'9
	bgt	2f
	dec	r3
	ble	prtood
	movb	r0,(r2)+
	inc	r1
	jsr	pc,get
	br	1b
/
/ got a non digit or EOF
/
2:
	tst	r1
	beq	prerr
1:
	mov	pfloat(sp),r1
	beq	4f
	cmp	r1,$1
	bne	1f
	cmp	r0,$'.
	beq	3f
	inc	r1
1:
	cmp	r1,$2
	bne	4f
2:
	cmp	r0,$'e
	beq	3f
	cmp	r0,$'E
	bne	4f
3:
	dec	r3
	ble	prtood
	movb	r0,(r2)+
	jsr	pc,get
	inc	r1
	mov	r1,pfloat(sp)
	sub	$2,r1
	beq	punsd
	jsr	pc,iosync
	bit	$EOF,flags
	bne	6f
	movb	*buf,r0
	br	psignd
4:
	dec	r3
	ble	prtood
	clrb	(r2)+
	rts	pc
prsec:
	sec
	rts	pc
prtood:
	mov	$ETOODIGITS,_perrno
	error	ETOODIGITS
prerr:
	tst	pfloat(sp)
	beq	1f
6:
	mov	$EBADFNUM,_perrno
	error	EBADFNUM
1:
	mov	$EBADINUM,_perrno
	error	EBADINUM
9:
	mov	$EPASTEOF,_perrno
	error	EPASTEOF
/
/ long atol(str)
/ char *str;
/
/ Convert string str to an integer
/
_atol:
	mov	2(sp),r3
	clr	r0
	clr	r1
	cmpb	(r3),$'-
	bne	1f
	inc	r3
1:
	movb	(r3)+,r2
	beq	1f
	ashc	$1,r0
	bvs	9f
	mov	r1,-(sp)
	mov	r0,-(sp)
	ashc	$2,r0
	bvs	9f
	add	(sp)+,r0
	bvs	9f
	add	(sp)+,r1
	adc	r0
	bvs	9f
	sub	$'0,r2
	add	r2,r1
	adc	r0
	bvs	9f
	br	1b
1:
	cmpb	*2(sp),$'-
	bne	1f
	neg	r0
	neg	r1
1:
	sbc	r0
	rts	pc
9:
	mov	$EBIGGIE,_perrno
	error	EBIGGIE
.bss
flags:	.=.+2
.text
