/
/

/ io4 -- numeric output conversion

qicv:
hicv:
	jsr	r5,rerr; 999.
	sys	exit

xicv:
	jsr	r5,fgetc
	rts	r5

gocv:
	mov	pc,gflg
	jsr	r5,getarg
	mov	ndig,_ndigit
	jsr	pc,ecvt
	tst	r2
	bmi	eocv
	cmp	r2,ndig
	bgt	eocv
	sub	r2,ndig
	sub	$4,width
	jsr	r5,focv
	add	$4,width
	add	$4,nspace
	rts	r5

eocv:
	mov	$'e,-(sp)
	br	1f

docv:
	mov	$'d,-(sp)
1:
	tst	gflg
	bne	1f
	jsr	r5,getarg
1:
	mov	ndig,r1
	add	$6,r1
	add	nflg,r1
	sub	width,r1
	bge	2f
	sub	r1,nspace
2:
	tst	nflg
	beq	2f
	mov	$'-,r0
	jsr	r5,fputcc
2:
	mov	ndig,r1
	mov	scale,r0
	bgt	2f
	add	r0,r1
	br	3f
2:
	inc	r1
3:
	mov	r1,_ndigit
	jsr	pc,ecvt
	mov	r0,r1
	mov	r2,-(sp)
	mov	scale,r2
	sub	r2,(sp)
	tst	r2
	bgt	2f
	mov	$'0,r0
	jsr	r5,fputcc
	br	3f
2:
	movb	(r1)+,r0
	dec	_ndigit
	jsr	r5,fputcc
	sob	r2,2b
3:
	mov	$'.,r0
	jsr	r5,fputcc
	neg	r2
	ble	2f
3:
	mov	$'0,r0
	jsr	r5,fputcc
	sob	r2,3b
2:
	mov	_ndigit,r2
	ble	2f
3:
	movb	(r1)+,r0
	jsr	r5,fputcc
	sob	r2,3b
2:
	mov	2(sp),r0
	jsr	r5,fputcc
	mov	(sp)+,r1
	bge	2f
	mov	$'-,r0
	jsr	r5,fputcc
	neg	r1
	br	3f
2:
	mov	$'+,r0
	jsr	r5,fputcc
3:
	clr	r0
	div	$10.,r0
	add	$'0,r0
	jsr	r5,fputcc
	mov	r1,r0
	add	$'0,r0
	jsr	r5,fputcc
	tst	(sp)+
	rts	r5

iocv:
	clr	-(sp)
	clr	ndig
	br	1f

focv:
	mov	$1,-(sp)
1:
	clr	-(sp)
	tst	gflg
	bne	1f
	jsr	r5,getarg
	tst	2(sp)
	beq	1f
	mov	scale,(sp)
1:
	mov	ndig,_ndigit
	add	(sp)+,_ndigit
	jsr	pc,fcvt
	mov	r0,r1
	tst	(sp)
	beq	1f
	tst	gflg
	bne	1f
	add	scale,r2
1:
	mov	ndig,r0
	add	(sp),r0
	add	nflg,r0
	tst	r2
	ble	1f
	add	r2,r0
	br	2f
1:
	inc	r0
2:
	sub	width,r0
	bge	1f
	sub	r0,nspace
1:
	tst	nflg
	beq	1f
	mov	$'-,r0
	jsr	r5,fputcc
1:
	tst	r2
	bgt	1f
	mov	$'0,r0
	jsr	r5,fputcc
	br	2f
1:
	movb	(r1)+,r0
	jsr	r5,fputcc
	sob	r2,1b
2:
	tst	(sp)+
	beq	1f
	mov	$'.,r0
	jsr	r5,fputcc
1:
	mov	ndig,-(sp)
	ble	1f
	tst	r2
	bge	1f
	neg	r2
2:
	mov	$'0,r0
	jsr	r5,fputcc
	dec	ndig
	ble	1f
	sob	r2,2b
1:
	mov	ndig,r2
	ble	2f
1:
	movb	(r1)+,r0
	jsr	r5,fputcc
	sob	r2,1b
2:
	mov	(sp)+,ndig
	rts	r5

getarg:
	clr	nflg
	setd
	cmpb	itype,$'r
	beq	1f
	seti
	cmpb	ilen,$4
	bne	2f
	setl
2:
	cmpb	ilen,$1
	beq	3f
	movif	*ilval,r0
	br	2f
3:
	movb	*ilval,r0
	movif	r0,fr0
	br	2f
1:
	cmpb	ilen,$4
	bne	1f
	movof	*ilval,r0
	br	2f
1:
	movf	*ilval,r0
2:
	cfcc
	bge	1f
	absf	r0
	mov	$1,nflg
1:
	rts	r5

