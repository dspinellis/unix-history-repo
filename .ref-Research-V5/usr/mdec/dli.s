/ dli -- DEC format  tape interpreter

.globl	fopen, fcreat, getc, putc, flush, mesg

	mov	(sp)+,r5
	cmp	r5,$1
	bne	1f
	jsr	r5,mesg; <Arg count\n\0>; .even
	sys	exit
1:
	tst	(sp)+
	mov	(sp)+,r0
	jsr	r5,fcreat; obuf
	bec	1f
	jsr	r5,mesg; <Cannot create\n\0>; .even
1:
	mov	$ppt,r0
	cmp	r5,$3
	blt	1f
	mov	(sp)+,r0
1:
	jsr	r5,fopen; ibuf
	bec	1f
	jsr	r5,mesg; <Cannot open\n\0>; .even
	sys	exit
1:
	jsr	pc,get
	tst	r0
	beq	1b
	cmp	r0,$1
	bne	err
	jsr	pc,get
	jsr	pc,get
	mov	r0,-(sp)
	jsr	pc,get
	swab	r0
	bis	r0,(sp)
	jsr	pc,get
	mov	r0,r1
	jsr	pc,get
	swab	r0
	bis	r0,r1
	sub	$6,(sp)
	ble	2f
	cmp	r1,origin
	beq	3f
	mov	r1,origin
	jsr	r5,flush; obuf
	mov	obuf,r0
	sys	seek; origin: 0; 0
3:
	jsr	pc,get
	jsr	pc,put
	dec	(sp)
	bgt	3b
	tst	(sp)+
	jsr	pc,get
	br	1b
2:
	jsr	r5,flush; obuf
	sys	exit

put:
	jsr	r5,putc; obuf
	inc	origin
	rts	pc

get:
	jsr	r5,getc; ibuf
	bec	1f

err:
	jsr	r5,flush; obuf
	mov	icount,r0
	jsr	pc,decml
	jsr	r5,mesg; < Input error\n\0>; .even
	sys	exit
1:
	inc	icount
	rts	pc

decml:
	mov	r0,r1
	clr	r0
	div	$10.,r0
	mov	r1,-(sp)
	tst	r0
	beq	1f
	jsr	pc,decml
1:
	mov	(sp)+,r0
	add	$'0,r0
	mov	r0,icount
	mov	$1,r0
	sys	write; icount; 1
	rts	pc

ppt:
	</dev/ppt\0>
	.even
	.bss

count:	.=.+2
ch:	.=.+2
icount:	.=.+2
ibuf:	.=.+520.
obuf:	.=.+520.

