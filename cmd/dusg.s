/ dusg -- summarize disk usage

	mov	(sp)+,r5
	tst	(sp)+
1:
	dec	r5
	bgt	2f
	tstb	buf1
	beq	3f
	sys	exit
3:
	mov	$dot,r0
	br	3f
2:
	mov	(sp)+,r0
3:
	cmpb	(r0),$'-
	bne	2f
	cmpb	1(r0),$'a
	bne	3f
	inc	aflg
	br	1b
3:
	cmpb	1(r0),$'s
	bne	1b
	dec	aflg
	br	1b
2:
	mov	$buf1,r1
2:
	movb	(r0)+,(r1)+
	bne	2b
	dec	r1
	clr	buf3+10.
	clr	icnt
	jsr	pc,tree
	tst	aflg
	bpl	1b
	jsr	r5,name
	br	1b

tree:
	sys	stat; buf1; buf2
	bes	1f
	cmp	buf2,$40.
	bgt	2f
1:
	clr	r4
	rts	pc
2:
	mov	icnt,r0
	mov	$ibuf,r2
	mov	buf2,r3
1:
	tst	r0
	beq	1f
	cmp	r3,(r2)+
	bne	2f
	clr	r4
	jsr	r5,cname
	rts	pc
2:
	dec	r0
	br	1b
1:
	mov	r3,(r2)+
	inc	icnt
	bit	$40000,buf2+2
	bne	1f
	jsr	pc,gsize
	jsr	r5,cname
	rts	pc
1:
	jsr	pc,gsize
	mov	r4,r3
	sys	open; buf1; 0
	bec	1f
	rts	pc
1:
	mov	r0,-(sp)
	mov	r1,-(sp)
1:
	mov	2(sp),r0
	sys	read; buf3; 10.
	bes	1f
	tst	r0
	beq	1f
	tst	buf3
	beq	1b
	cmp	buf3+2,$".\0
	beq	1b
	cmp	buf3+2,$"..
	bne	2f
	tst	buf3+4
	beq	1b
2:
	mov	$buf3+2,r2
	mov	(sp),r1
	movb	$'/,(r1)+
	cmpb	-2(r1),$'/
	bne	2f
	dec	r1
2:
	movb	(r2)+,(r1)+
	bne	2b
	dec	r1
	mov	r3,-(sp)
	jsr	pc,tree
	mov	r4,r3
	add	(sp)+,r3
	br	1b
1:
	mov	(sp)+,r1
	clrb	(r1)
	mov	(sp)+,r0
	sys	close
	mov	r3,r4
	tst	aflg
	bmi	1f
	jsr	r5,name
1:
	rts	pc

cname:
	tst	aflg
	bgt	name
	rts	r5

name:
	jsr	pc,pnum
	mov	$011,r0
	jsr	pc,pchar
	mov	$buf1,r2
1:
	movb	(r2)+,r0
	beq	1f
	jsr	pc,pchar
	br	1b
1:
	mov	$'\n,r0
	jsr	pc,pchar
	rts	r5

gsize:
	mov	r5,-(sp)
	mov	buf2+6,r5		/ size
	add	$511.,r5
	clr	r4
	alsc	$-9,r4
	cmp	r5,$8.
	blo	1f
	mov	r5,-(sp)
	add	$255.,r5
	alsc	$-8,r4
	add	(sp)+,r5
1:
	mov	r5,r4
	mov	(sp)+,r5
	rts	pc

pnum:
	mov	r4,-(sp)
	mov	r5,-(sp)
	mov	r4,r5
	jsr	pc,1f
	mov	(sp)+,r5
	mov	(sp)+,r4
	rts	pc
1:
	clr	r4
	dvd	$10.,r4
	mov	r5,-(sp)
	mov	r4,r5
	beq	1f
	jsr	pc,1b
1:
	mov	(sp)+,r0
	add	$'0,r0
	jsr	pc,pchar
	rts	pc

pchar:
	mov	r0,0f
	mov	$1,r0
	sys	write; 0f; 1
	rts	pc
0:	0

dot:	<.\0>

	.bss

icnt:	.=.+2
buf1:	.=.+100.
buf2:	.=.+40.
buf3:	.=.+12.
aflg:	.=.+2
ibuf:	.=.+1000.
,r5
	clr	r4
	alsc	$-9,r4
	cmp	r5,$8.
	blo	1f
	mov	r5,-(sp)
	add	$255.,r5
	alsc	$-8,r4