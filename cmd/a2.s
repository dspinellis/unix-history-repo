/ a2 -- pdp-11 assembler pass 1

error:
	incb	errflg
	mov	r0,-(sp)
	mov	r1,-(sp)
	mov	(r5)+,r0
	mov	*curarg,0f
	beq	1f
	clr	*curarg
	mov	r0,-(sp)
	jsr	r5,filerr; 0:0; '\n
	mov	(sp)+,r0
1:
	mov	r2,-(sp)
	mov	r3,-(sp)
	mov	line,r3
	movb	r0,1f
	mov	$1f+6,r0
	mov	$4,r1
2:
	clr	r2
	dvd	$10.,r2
	add	$'0,r3
	movb	r3,-(r0)
	mov	r2,r3
	sob	r1,2b
	mov	$1,r0
	sys	write; 1f; 7
	mov	(sp)+,r3
	mov	(sp)+,r2
	mov	(sp)+,r1
	mov	(sp)+,r0
	rts	r5

1:	<f xxxx\n>
	.even

betwen:
	cmp	r0,(r5)+
	blt	1f
	cmp	(r5)+,r0
	blt	2f
1:
	tst	(r5)+
2:
	rts	r5

putw:
	tst	ifflg
	beq	1f
	cmp	r4,$'\n
	bne	2f
1:
	jsr	pc,putc1
	swab	r4
	jsr	pc,putc1
	swab	r4
2:
	rts	pc

putc:
	tst	ifflg
	bne	1f
putc1:
	movb	r4,*obufp
	inc	obufp
	cmp	obufp,$outbuf+512.
	blo	1f
	mov	$outbuf,obufp
	movb	pof,r0
	sys	write; outbuf; 512.
1:
	rts	pc

2
	add	$'0,r3
	movb	r3,-(r0)
	mov	r2,r3
	sob	r1,2b
	mov	$1,r0
	sys	write; 1f; 7
	mov	(sp)+,r3
	mov	(sp)+,r2
	mov	(sp)+,r1
	mov	(sp)+,r0
	rts	r5

1:	<f xxxx\n>
	.even

betwen:
	cmp	r0,(r5)+
	blt	1f
	cmp	(r5)+,r0
	blt	2f
1:
	t