/ clri -- clear inode

	cmp	(sp)+,$2
	blt	error
	beq	1f
	mov	4(sp),fs1
	mov	4(sp),fs2
1:
	tst	(sp)+
	mov	(sp)+,r0
	clr	r5
1:
	movb	(r0)+,r1
	beq	1f
	mpy	$10.,r5
	sub	$'0,r1
	cmp	r1,$10.
	bhis	error
	add	r1,r5
	br	1b
1:
	add	$31.,r5
	mov	r5,r0
	als	$-4,r0
	mov	r0,0f
	sys	open; fs1: filsys; 0
	bes	error
	mov	r0,-(sp)
	sys	seek; 0:..; 3
	mov	(sp),r0
	sys	read; buf; 512.
	mov	(sp)+,r0
	sys	close
	mov	r5,r0
	als	$5,r0
	bic	$!777,r0
	add	$buf,r0
	mov	$16.,r5
1:
	clr	(r0)+
	sob	r5,1b
	sys	open; fs2: filsys; 1
	bes	error
	mov	r0,-(sp)
	mov	0b,0f
	sys	seek; 0:..; 3
	mov	(sp)+,r0
	sys	write; buf; 512.
	bes	error
	sys	exit

error:
	mov	$1,r0
	sys	write; 1f; 2f-1f
	sys	exit
1:
	<error\n>
2:

filsys:	</dev/junk\0>
.even
	.bss
buf:	.=.+512.
