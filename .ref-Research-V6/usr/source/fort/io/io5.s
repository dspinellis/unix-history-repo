/
/

/ io5 -- more conversions

/.globl	hocv
/.globl	qocv
/.globl	xocv
/.globl	aocv
/.globl	locv
/
/.globl	fmtchr
/.globl	fputcc
/.globl	rep
/.globl	formp
/.globl	spaces
/.globl	ilen
/.globl	width
/.globl	ilval
.globl	rerr

hocv:
	jsr	r5,fmtchr
	tst	r0
	beq	2f
	jsr	r5,fputcc
	dec	rep
	bgt	hocv
	rts	r5
2:
	jsr	r5,rerr; 111.
	sys	exit

qocv:
	mov	formp,-(sp)
1:
	jsr	r5,fmtchr
	tst	r0
	beq	2f
	cmp	r0,$'"
	beq	2f
	jsr	r5,fputcc
	br	1b
2:
	dec	rep
	ble	1f
	mov	(sp),formp
	br	1b
1:
	tst	(sp)+
	rts	r5

xocv:
	mov	$1,r1
	jsr	r5,spaces
	rts	r5

aocv:
	movb	ilen,r1
	sub	width,r1
	neg	r1
	bpl	1f
	clr	r1
1:
	jsr	r5,spaces
	mov	ilval,r2
	mov	width,r1
	cmpb	r1,ilen
	ble	2f
	movb	ilen,r1
2:
	movb	(r2)+,r0
	jsr	r5,fputcc
	dec	r1
	bgt	2b
	rts	r5

locv:
	mov	width,r1
	dec	r1
	jsr	r5,spaces
	mov	$'f,r0
	movb	ilen,r1
	mov	ilval,r2
2:
	tstb	(r2)+
	bne	1f
	dec	r1
	bgt	2b
	br	2f
1:
	mov	$'t,r0
2:
	jsr	r5,fputcc
	rts	r5

