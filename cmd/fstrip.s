/ fstrip -- remove fortran internal symbols

.globl	fopen
.globl	getw
.globl	putw
.globl	flush

	mov	(sp)+,argc
	tst	(sp)+
	br	loop
unloop:
	sys	unlink; s.tmp
loop:
	dec	argc
	bgt	1f
	sys	exit
1:
	mov	(sp)+,r4
	mov	r4,r0
	jsr	r5,fopen; ibuf
	bcc	1f
	jsr	r5,mesg; <open error: \0>; .even
	br	loop
1:
	mov	$'a,r1
1:
	movb	r1,s.tmp+8
	sys	stat; s.tmp; obuf
	bec	2f
	sys	creat; s.tmp; 10
	bec	1f
2:
	inc	r1
	cmp	r1,$'z
	blos	1b
	jsr	r5,mesg; <can't create temp file for \0>; .even
	sys	exit
1:
	mov	r0,obuf
	clr	obuf+2
	clr	obuf+4
	jsr	r5,getw; ibuf
	cmp	r0,magic
	beq	1f
	jsr	r5,mesg; <improper format: \0>; .even
	br	unloop
1:
	jsr	r5,putw; obuf
	jsr	r5,getw; ibuf
	mov	r0,r2
	jsr	r5,putw; obuf
	jsr	r5,getw; ibuf
	add	r0,r2
	jsr	r5,putw; obuf
	jsr	r5,getw; ibuf
	jsr	r5,putw; obuf
	jsr	r5,getw; ibuf
	jsr	r5,putw; obuf
	jsr	r5,getw; ibuf
	jsr	r5,putw; obuf
	jsr	r5,getw; ibuf
	jsr	r5,putw; obuf
	jsr	r5,getw; ibuf
	cmp	r0,$1
	bne	9f
	asr	r2		/ no relocation
9:
	jsr	r5,putw; obuf
1:
	jsr	r5,getw; ibuf
	bcc	2f
	jsr	r5,mesg; <unexpected EOF: \0>; .even
	sys	exit
2:
	jsr	r5,putw; obuf
	dec	r2
	bne	1b
	clr	r2
1:
	jsr	r5,getsym
	bcs	1f
	cmpb	symbol+1,$'0
	blo	3f		/ not digit
	cmpb	symbol+1,$'9
	bhi	3f
	cmpb	symbol,$'t
	beq	1b
	cmpb	symbol,$'d
	beq	1b
	cmpb	symbol,$'c
	beq	1b
3:
	jsr	r5,putsym
	add	$14,r2
	br	1b
1:
	jsr	r5,flush; obuf
	mov	obuf,r0
	sys	close
	mov	ibuf,r0
	sys	close
	mov	r4,0f
	sys	creat; 0:..; 0		/ same mode as before
	bec	1f
	jsr	r5,mesg; <can't rewrite: \0>; .even
	jmp	unloop
1:
	mov	r0,ibuf
	sys	open; s.tmp; 0
	bec	1f
	jsr	r5,mesg; <can't read temp file for: \0>; .even
	sys	exit
1:
	mov	r0,ibuf+2
	sys	read; obuf; 512.
	mov	r2,obuf+10	/ new data
	br	2f
1:
	mov	ibuf+2,r0
	sys	read; obuf; 512.
2:
	mov	r0,0f
	beq	1f
	mov	ibuf,r0
	sys	write; obuf; 0:..
	br	1b
1:
	mov	ibuf,r0
	sys	close
	mov	ibuf+2,r0
	sys	close
	jmp	unloop

mesg:
	movb	(r5)+,ch
	beq	1f
	mov	$1,r0
	sys	write; ch; 1
	br	mesg
1:
	inc	r5
	bic	$1,r5
	mov	r4,r1
1:
	movb	(r1)+,ch
	beq	1f
	mov	$1,r0
	sys	write; ch; 1
	br	1b
1:
	mov	$1,r0
	sys	write; qnl; 1
	rts	r5

getsym:
	mov	r5,-(sp)
	mov	$symbol,r5
	mov	$6,-(sp)
1:
	jsr	r5,getw; ibuf
	bcs	2f
	mov	r0,(r5)+
	dec	(sp)
	bne	1b
	tst	(sp)+
	mov	(sp)+,r5
	rts	r5
2:
	tst	(sp)+
	mov	(sp)+,r5
	sec
	rts	r5

putsym:
	mov	r5,-(sp)
	mov	$symbol,r5
	mov	$6,-(sp)
1:
	mov	(r5)+,r0
	jsr	r5,putw; obuf
	dec	(sp)
	bne	1b
	tst	(sp)+
	mov	(sp)+,r5
	rts	r5

s.tmp:
	</tmp/stma\0>
qnl:
	<\n>
	.even
magic:
	br	.+20

.bss
argc:	.=.+2
symbol:	.=.+14
ibuf:	.=.+518.
obuf:	.=.+518.
ch:	.=.+2

; 1
	br	1b
1:
	mov	$1,r0
	sys	write; qnl; 1
	rts