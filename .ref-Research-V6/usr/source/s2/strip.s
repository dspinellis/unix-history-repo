/ strip -- strip relocation and symbols

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
	mov	r4,0f
	sys	open; 0:..; 0
	bec	1f
	jsr	r5,mesg; <open error: \0>; .even
	br	loop
1:
	mov	r0,fi
	mov	$'a,r1
1:
	movb	r1,s.tmp+8
	sys	stat; s.tmp; buf
	bec	2f
	sys	creat; s.tmp; 400
	bec	1f
2:
	inc	r1
	cmp	r1,$'z
	blos	1b
	jsr	r5,mesg; <can't create temp file for \0>; .even
	sys	exit
1:
	mov	r0,fo
	clr	buf
	mov	fi,r0
	sys	read; buf; 512.
	mov	r0,r3
	cmp	buf,magic
	beq	1f
	cmp	buf,magic1
	beq	1f
	cmp	buf,magic2
	beq	1f
	jsr	r5,mesg; <improper format: \0>; .even
	br	unloop
1:
	mov	buf+2,r2
	add	buf+4,r2
	add	$20,r2
	clr	r1
	clr	buf+10
	mov	$1,buf+16		/ no reloc bits flag
1:
	mov	r2,0f
	sub	r1,0f
	cmp	0f,r3
	blos	2f
	mov	r3,0f
2:
	mov	fo,r0
	sys	write; buf; 0:..
	add	0b,r1
	cmp	r1,r2
	bhis	1f
	mov	fi,r0
	sys	read; buf; 512.
	mov	r0,r3
	bne	1b
	jsr	r5,mesg; <unexpected EOF: \0>; .even
1:
	mov	fo,r0
	sys	close
	mov	fi,r0
	sys	close
	mov	r4,0f
	sys	creat; 0:..; 0		/ same mode as before
	bec	1f
	jsr	r5,mesg; <can't rewrite: \0>; .even
	jmp	unloop
1:
	mov	r0,fo
	sys	open; s.tmp; 0
	bec	1f
	jsr	r5,mesg; <can't read temp file for: \0>; .even
	sys	exit
1:
	mov	r0,fi
1:
	mov	fi,r0
	sys	read; buf; 512.
	mov	r0,0f
	beq	1f
	mov	fo,r0
	sys	write; buf; 0:..
	br	1b
1:
	mov	fi,r0
	sys	close
	mov	fo,r0
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

s.tmp:
	</tmp/stma\0>
qnl:
	<\n>
	.even
magic:	407
magic1:	410
magic2:	411

.bss
fi:	.=.+2
fo:	.=.+2
argc:	.=.+2
buf:	.=.+512.
ch:	.=.+2

