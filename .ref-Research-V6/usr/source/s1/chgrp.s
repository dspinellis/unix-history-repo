/ chgrp -- change group


	.globl	fopen, getc, mesg

	mov	sp,r5
	mov	(r5),r4
	cmp	r4,$3
	bge	1f
	jsr	r5,mesg; <chown uid f1 ...\n\0>; .even
1:
	add	$4,r5
	mov	(r5),r3
	cmpb	(r3),$'0
	blt	1f
	cmpb	(r3),$'9
	bgt	1f
	jsr	r5,cvnum; geta
	br	do
1:
	mov	$uids,r0
	jsr	r5,fopen; ubuf
	bec	1f
	jsr	r5,mesg; <Can't open /etc/uids\n\0>; .even
	sys	exit
1:
	mov	r3,r2
2:
	jsr	r5,getc; ubuf
	bcc	3f
who:
	jsr	r5,mesg; <Who?\n\0>; .even
	sys	exit
3:
	cmp	r0,$':
	beq	3f
	cmpb	(r2)+,r0
	beq	2b
2:
	jsr	r5,getc; ubuf
	bcs	who
	cmp	r0,$'\n
	bne	2b
	br	1b
3:
	tstb	(r2)
	bne	2b
3:
	jsr	r5,getc; ubuf
	cmpb	r0,$':
	bne	3b
	jsr	r5,cvnum; getc
do:
	sub	$2,r4
	swab	r1
	mov	r1,0f+2
	tst	(r5)+
1:
	mov	(r5),0f
	mov	(r5)+,9f
	sys	stat; 9:..; statb
	movb	statb+7,0f+2
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
	movb	(r3)+,r0
	tst	(r5)+
	rts	r5

uids:	</etc/group\0>

	.bss
statb:	.=.+36.
ubuf:	.=.+518.
