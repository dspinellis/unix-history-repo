/ copy mag tape to disk

	jsr	pc,rew
2:
	jsr	pc,4(r5)
		<'p' for rp; 'k' for rk\n\0>
		.even
	jsr	pc,2(r5)
	cmp	r0,$'k
	bne	3f
	mov	$rkblk,r0
	br	1f
3:
	cmp	r0,$'p
	bne	2b
	mov	$rpblk,r0
1:
	mov	r0,rxblk
	mov	$'\n,r0
	jsr	pc,(r5)
	jsr	pc,4(r5)
		<disk offset\n\0>
		.even
	jsr	pc,numb
	mov	r0,bno
	jsr	pc,4(r5)
		<tape offset\n\0>
		.even
	jsr	pc,numb
	mov	r0,r1
	beq	2f
1:
	jsr	pc,rblk
	dec	r1
	bne	1b
2:

	jsr	pc,4(r5)
		<count\n\0>
		.even
	jsr	pc,numb
	mov	r0,r1
1:
	jsr	pc,rblk
	jsr	pc,wblk
	dec	r1
	bne	1b
	jsr	pc,rew
	rts	pc

numb:
	clr	r1
1:
	jsr	pc,2(r5)
	cmp	r0,$'\n
	beq	1f
	sub	$'0,r0
	cmp	r0,$9
	bhi	2f
	mul	$10.,r1
	add	r0,r1
	br	1b
1:
	mov	r1,r0
	rts	pc
2:
	jsr	pc,4(r5)
		<illegal digit\n\0>
		.even
	tst	(sp)+
	rts	pc

mts = 172520
mtc = 172522
mtbrc = 172524
mtcma = 172526

rblk:
	bit	$2,*$mts
	bne	rblk
	tstb	*$mtc
	bge	rblk
	mov	$-512.,*$mtbrc
	mov	$buf,*$mtcma
	mov	$60003,*$mtc
1:
	tstb	*$mtc
	bge	1b
	tst	*$mtc
	bge	1f
	jsr	pc,4(r5)
		<tape error\n\0>
		.even
	mov	$-1,*$mtbrc
	mov	$60013,*$mtc
	br	rblk
1:
	rts	pc

rew:
	mov	$60017,*$mtc
	rts	pc

wblk:
	mov	bno,r0
	inc	bno
	mov	r1,-(sp)
	mov	r0,r1
	clr	r0
	jmp	*rxblk

rpda = 176724
rpblk:
	div	$10.,r0
	mov	r1,-(sp)
	mov	r0,r1
	clr	r0
	div	$20.,r0
	bisb	r1,1(sp)
	mov	$rpda,r1
	mov	(sp)+,(r1)
	mov	r0,-(r1)
	br	1f

rkda = 177412
rkblk:
	div	$12.,r0
	ash	$4.,r0
	bis	r1,r0
	mov	$rkda,r1
	mov	r0,(r1)

1:
	mov	$buf,-(r1)
	mov	$-256.,-(r1)
	mov	$3,-(r1)
1:
	tstb	(r1)
	bge	1b
	tst	(r1)
	blt	1f
	mov	(sp)+,r1
	rts	pc
1:
	jsr	pc,4(r5)
		<disk error\n\0>
		.even
	mov	(sp)+,r1
	dec	bno
	br	wblk

.bss
buf:	.=.+512.
rxblk:	.=.+2
bno:	.=.+2
