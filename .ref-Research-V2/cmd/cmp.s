/ cmp -- compare files


	cmp	(sp)+,$3
	beq	1f
	jsr	r5,mesg; <Usage: cmp arg1 arg2\n\0>; .even
	sys	exit
1:
	tst	(sp)+
	mov	(sp)+,0f
	sys	open; 0:..; 0
	bec	1f
	jsr	r5,mesg; <Can't open arg1.\n\0>; .even
	sys	exit
1:
	mov	r0,f1
	mov	(sp)+,0f
	sys	open; 0:..; 0
	bec	1f
	jsr	r5,mesg; <Can't open arg2.\n\0>; .even
	sys	exit
1:
	mov	r0,f2
	clr	r2
1:
	jsr	r5,getw; f1
	bvs	eof1
	mov	r0,r3
	jsr	r5,getw; f2
	bvs	eof2
	cmp	r0,r3
	beq	2f
	mov	r0,r4
	mov	r2,r0
	jsr	pc,octal
	jsr	r5,mesg; <: \0>; .even
	mov	r3,r0
	jsr	pc,octal
	jsr	r5,mesg; < \0>;
	mov	r4,r0
	jsr	pc,octal
	jsr	r5,mesg; <\n\0>
2:
	add	$2,r2
	br	1b
eof1:
	jsr	r5,getw; f2
	bvs	1f
	jsr	r5,mesg; <EOF on arg1.\n\0>; .even
	sys	exit
1:
	sys	exit

eof2:
	jsr	r5,mesg; <EOF on arg2.\n\0>; .even
	sys	exit

mesg:
	movb	(r5)+,ch
	beq	1f
	mov	$1,r0
	sys	write; ch; 1
	br	mesg
1:
	inc	r5
	bic	$1,r5
	rts	r5

getw:
	mov	(r5)+,r1
	cmp	2(r1),$1
	bne	1f
	mov	*4(r1),r0
	bic	$!377,r0
	dec	2(r1)
	rts	r5
1:
	sub	$2,2(r1)
	bge	1f
	mov	(r1),r0
	mov	r1,0f
	add	$6,0f
	sys	read; 0:..; 512.
	mov	r0,2(r1)
	bne	2f
	sev
	rts	r5
2:
	mov	r1,4(r1)
	add	$6,4(r1)
	sub	$2,2(r1)
1:
	mov	*4(r1),r0
	add	$2,4(r1)
	rts	r5

octal:
	mov	r4,-(sp)
	mov	r0,r5
	mov	$6,-(sp)
1:
	clr	r4
	alsc	$1,r4
	cmp	(sp),$6
	beq	2f
	alsc	$2,r4
2:
	add	$'0,r4
	mov	r4,ch
	mov	$1,r0
	sys	write; ch; 1
	dec	(sp)
	bne	1b
	tst	(sp)+
	mov	(sp)+,r4
	rts	pc

	.bss

ch:	.=.+2
f1:	.=.+6; .=.+512.
f2:	.=.+6; .=.+512.
*4(r1),r0
	bic	$!377,r0
	dec	2(r1)
	rts	r5
1:
	sub	$2,2(r1)
	bge	1f
	mov	(r1),r0
	mov	r1,0f
	add	$6,0f
	sys	read; 0:..