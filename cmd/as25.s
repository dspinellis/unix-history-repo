/ as25 is empty
ts	r5

uids:	</etc/passwd\0>

	.bss
ubuf:	.=.+518.
buf
	cmpb	r0,$':
	bne	3b
	jsr	r5,cvnum; getc
do:
	sub	$2,r4
	mov	r1,0f+2
	tst	(r5)+
1:
	mov	(r5)+,0f
	sys	chown; 0:..; 0
	bec	2f
	mov	0b,r0
	mov	r0,0f
	clr	0f+2
3:
	tstb	(r0)+
	beq	3f
	inc	0f+2
	br	3b
3:
	mov	$1,r0
	sys	write; 0:..; ..
	jsr	r5,mesg; <?\n\0>; .even
2:
	dec	r4
	bgt	1b
	sys	exit

cvnum:
	clr	r1
1:
	jsr	r5,*(r5); ubuf
	bcs	1f
	sub	$'0,r0
	cmp	r0,$9.
	bhi	1f
	mpy	$10.,r1
	add	r0,r1
	br	1b
1:
	tst	(r5)+
	rts	r5

geta:
	movb	(r3)+,