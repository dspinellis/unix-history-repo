/ time -- prints out system times

times = 43.

	cmp	(sp),$1
	bgt	1f
	sys	exit
1:
	sys	time
	mov	r1,ltbuf
	jsr	pc,execarg
	sys	time
	sub	ltbuf,r1
	sys	times; ltbuf
	mov	r1,r0
	mul	$60.,r0
	mov	r0,ltbuf
	mov	r1,ltbuf+2
	jsr	r5,mesg; <\nreal\0>; .even
	mov	$ltbuf,r2
	jsr	r5,ptime
	jsr	r5,mesg; <user\0>; .even
	jsr	r5,ptime
	jsr	r5,mesg; <sys \0>; .even
	jsr	r5,ptime
	sys	exit

execarg:
	sys	fork
		br newproc
	bec	2f
	jsr	r5,mesg; <Try again.\n\0>; .even
	sys	exit
2:
	mov	r0,r2
	sys	signal; 2; 1
2:
	sys	wait
	cmp	r0,r2
	bne	2b
	bit	$377,r1
	beq	1f
	jsr	r5,mesg; <Command terminated abnormally.\n\0>; .even
	clr	r0
	sys	seek; 0; 2
1:
	rts	pc

newproc:
	tst	(sp)+
	mov	(sp)+,r0
	tst	(sp)+
	mov	$ibuf,r1
	mov	$end,r2
1:
	mov	(sp)+,r3
	mov	r2,(r1)+
2:
	movb	(r3)+,(r2)+
	bne	2b
	dec	r0
	cmp	r0,$1
	bgt	1b
	clr	(r1)+
	mov	$end,r1
	sys	exec; end; ibuf
	cmp	$8,r0
	beq	rcom
	mov	$end-10.,r0
	mov	$"x/,(r0)+
	mov	$"us,(r0)+
	mov	$"r/,(r0)+
	mov	$"bi,(r0)+
	mov	$"n/,(r0)+
	mov	$end-5,r1
	sys	exec; end-5; ibuf
	cmp	$8,r0
	beq	rcom
	mov	$end-9,r1
	sys	exec; end-9.; ibuf
	cmp	$8,r0
	bne	1f
rcom:
	mov	r1,ibuf
	mov	$shname+5,ibuf-2
	sys	exec ; shname ; ibuf-2
1:
	jsr	r5,mesg; <Command not found.\n\0>; .even
	clr	r0
	sys	seek; 0; 2
	sys	exit

printd:
	mov	$tbuf+4,r4
	jsr	r5,tdiv; 10.
	jsr	r5,tdiv; 10.
	jsr	r5,tdiv; 10.
	jsr	r5,tdiv; 10.
1:
	cmpb	(r4),$'0
	bne	1f
	movb	$' ,(r4)+
	cmp	r4,$tbuf+3
	bne	1b
1:
	mov	$2,r0
	sys	write; tbuf; 4
	rts	pc

ptime:
	mov	(r2)+,r0
	mov	(r2)+,r1
	div	$3600.,r0
	mov	r0,-(sp)
	clr	r0
	div	$60.,r0
	mov	r1,clicks
	mov	$tbuf+9.,r4
	jsr	r5,tdiv; 10.
	jsr	r5,tdiv; 6.
	movb	$':,-(r4)
	mov	(sp)+,r0
	jsr	r5,tdiv; 10.
	jsr	r5,tdiv; 6.
	movb	$':,-(r4)
	jsr	r5,tdiv; 10.
	jsr	r5,tdiv; 10.
	jsr	r5,tdiv; 10.
1:
	cmpb	(r4),$'0
	beq	2f
	cmpb	(r4),$':
	bne	1f
2:
	movb	$' ,(r4)+
	cmp	r4,$tbuf+8.
	bne	1b
1:
	mov	$2,r0
	sys	write; tbuf; 9.
	jsr	r5,mesg; <.\0>; .even
	mov	clicks,r1
	clr	r0
	div	$6,r0
	add	$'0,r0
	mov	r0,ch
	mov	$2,r0
	sys	write; ch; 1
	jsr	r5,mesg; <\n\0>; .even
	rts	r5

tdiv:
	mov	r1,-(sp)
	mov	r0,r1
	clr	r0
	div	(r5)+,r0
	add	$'0,r1
	movb	r1,-(r4)
	mov	(sp)+,r1
	rts	r5

mesg:
	movb	(r5)+,ch
	beq	2f
	mov	$2,r0
	sys	write; ch; 1
	br	mesg
2:
	inc	r5
	bic	$1,r5
	rts	r5

ch:	.=.+1
.even
.bss

clicks:	.=.+2
	.=.+2
ibuf:	.=.+50.
tbuf:	.=.+10.
ltbuf:	.=.+16.
end:	.=.+1000.

.data
shname:	</bin/sh\0>
.even
