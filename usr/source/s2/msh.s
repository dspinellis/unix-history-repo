/ ms -- mini-shell

	mov	$sbuf,r1
	mov	$cbuf,r2
	clr	r3
	jsr	pc,nonblank
	cmp	r0,$'\n
	bne	loop
	sys	exit

loop:
	mov	r1,(r2)+
1:
	movb	r0,(r1)+
	jsr	pc,getc
	cmp	r0,$' /
	beq	1f
	cmp	r0,$'\n
	beq	go
	br	1b
1:
	clrb	(r1)+
	jsr	pc,nonblank
	cmp	r0,$'\n
	beq	go
	br	loop

go:
	clr	(r2)+
	tst	r3	/ to call glob
	bne	1f
	sys	exec; sbuf; cbuf
	mov	$bn,r0
	mov	$"/b,(r0)+
	mov	$"in,(r0)+
	movb	$'/,(r0)+
	sys	exec; bn; cbuf
	mov	$usr,r0
	mov	$"/u,(r0)+
	mov	$"sr,(r0)+
	sys	exec; usr; cbuf
	br	error
1:
	mov	$gl,cbuf-2
	sys	exec; gl; cbuf-2

error:
	mov	$1,r0
	sys	write; 1f; 2
	sys	exit
1:	<?\n>

nonblank:
	jsr	pc,getc
	cmp	r0,$' /
	beq	nonblank
	rts	pc

getc:
	clr	r0
	sys	read; ch; 1
	bes	1f
	tst	r0
	beq	1f
	movb	ch,r0
	cmp	r0,$'?
	beq	3f
	cmp	r0,$'*
	beq	3f
	cmp	r0,$'[
	bne	2f
3:
	inc	r3
2:
	rts	pc
1:
	sys	exit

ch:	.=.+1
gl:	</etc/glob\0>
	.even

.bss
usr:	.=.+4		/ room for /usr
bn:	.=.+5		/ room for /bin/
sbuf:	.=.+1000.
	.even
	.=.+2	/ slot for ptr to glob
cbuf:	.=.+20.

