/ df -- find free space

	cmp	(sp)+,$1
	bgt	1f
	mov	$rf0,0f
	jsr	pc,df
	mov	$1,r0
	sys	write; plus; 1
	mov	$rk1,0f
	jsr	pc,df
	mov	$1,r0
	sys	write; plus; 1
	mov	$rk2,0f
	jsr	pc,df
	mov	$1,r0
	sys	write; plus; 1
	mov	$rk3,0f
	jsr	pc,df
	mov	$1,r0
2:
	mov	$1,r0
	sys	write; nl; 1
	sys	exit

1:
	tst	(sp)+
	mov	(sp)+,0f
	jsr	pc,df
	br	2b

df:
	clr	r3
	sys	36.
	sys	open; 0:..; 0
	bes	9f
	sys	read; nfree; 1024.
	mov	$freeb,r1
	mov	nfree,r2
	asr	r2
1:
	mov	$16.,r4
	mov	(r1)+,r5
2:
	rol	r5
	adc	r3
	dec	r4
	bne	2b
	dec	r2
	bgt	1b
9:
	clr	r2
	dvd	$10.,r2
	mov	r3,-(sp)
	mov	r2,r3
	beq	2f
	jsr	pc,9b
2:
	movb	(sp)+,ch
	add	$'0,ch
	mov	$1,r0
	sys	write; ch; 1
	rts	pc

rf0:	</dev/rf0\0>
rk0:	</dev/rk0\0>
rk1:	</dev/rk1\0>
rk2:	</dev/rk2\0>
rk3:	</dev/rk3\0>
plus:	<+>
nl:	<\n>
	.even

	.bss
ch:	.=.+2
nfree:	.=.+2
freeb:	.=.+1022.

0f
	jsr	pc,df
	br	2b

df:
	clr	r3
	sys	36.
	sys	open; 0:..; 0
	bes	9f
	sys	read; nfree; 1024.
	mov	$freeb,r1
	mov	nfree,r2
	asr	r2
1:
	mov	$16.,r4
	mov	(r1)+,r5
2:
	rol	r5
	adc	r3
	dec	r4
	bne	2b
	