/ dsw - delete from tty

	cmp	(sp)+,$2
	blt	1f
	tst	(sp)+
	mov	(sp)+,0f
1:
	sys	stat; 0:dot; stbuf
	bes	error
	bit	$40000,stbuf+2
	beq	error
	mov	0b,0f
	sys	open; 0:..; 0
	bes	error
	mov	r0,r1
1:
	clrb	buf+10.
	mov	r1,r0
	sys	read; buf; 10.
	bes	done
	tst	r0
	beq	done
	tst	buf
	beq	1b
	mov	0b,r2
	mov	$obuf,r3
2:
	movb	(r2)+,(r3)+
	bne	2b
	mov	$buf+2,r2
	dec	r3
	cmpb	-1(r3),$'/
	beq	2f
	movb	$'/,(r3)+
2:
	movb	(r2)+,(r3)+
	bne	2b
	sys	stat; obuf; stbuf
	bes	error
	bit	$40000,stbuf+2
	bne	1b
	mov	$buf+2,r2
2:
	tstb	(r2)+
	bne	2b
	movb	$' ,-(r2)
	sub	$buf+1,r2
	mov	r2,0f

2:
	mov	$1,r0
	sys	write; buf+2; 0:..
	clr	r0
	sys	read; ch; 1
	cmpb	ch,$'\n
	beq	1b
	clr	r0
	sys	read; ch1; 1
	cmpb	ch1,$'\n
	beq	3f
4:
	clr	r0
	sys	read; ch; 1
	cmpb	ch,$'\n
	beq	2b
	br	4b
3:
	cmpb	ch,$'x
	beq	done
	cmpb	ch,$'y
	bne	2b
	sys	unlink; obuf
	bes	error
	br	1b

done:
	sys	exit

error:
	mov	$1,r0
	sys	write; mes; 2
	sys	exit

dot:	<.\0>
mes:	<?\n>

	.bss
obuf:	.=.+100.
stbuf:	.=.+40.
buf:	.=.+11.
ch:	.=.+1
ch1:	.=.+1

v	$buf+2,r2
2: