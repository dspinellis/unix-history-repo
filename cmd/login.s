/  login --  enter new user

.globl	ttyn
.globl	crypt
.globl	fopen
.globl	getc
.globl	mesg

	sys	quit; 0
	sys	intr; 0
	jsr	pc,ttyn
	movb	r0,ttyx+8.
	sub	$'0,r0
	cmp	r0,$'a-'0
	blo	1f
	sub	$'a-'0-10.,r0	/ map a-z into 10. on
1:
	asl	r0
	asl	r0
	asl	r0
	asl	r0
	mov	r0,offset
	mov	(sp)+,r5
	tst	(sp)+
	dec	r5
	ble	login
	mov	(sp)+,r4
	mov	$uname,r1
2:
	movb	(r4)+,(r1)+
	bne	2b
	dec	r5
	ble	login
	mov	(sp)+,r4
	mov	$passwd,r1
2:
	movb	(r4)+,(r1)+
	bne	2b
login:
	clrb	uname+8.
	mov	$passwdf,r0
	jsr	r5,fopen; pbuf
	bec	1f
	jsr	r5,mesg; <Can't open password file\n\0>; .even
	sys	exit
1:
	jsr	pc,guname
1:
	jsr	r5,compar; uname
		br .+4
	br	2f
3:
	jsr	r5,getc; pbuf
	bes	sorry
	cmp	r0,$'\n
	bne	3b
	br	1b
sorry:
	jsr	r5,mesg; <Login incorrect\n\0>; .even
	mov	pbuf,r0
	sys	close
	clr	uname
	clr	passwd
	br	login
2:
	jsr	r5,getc; pbuf
	cmp	r0,$':
	beq	2f
	mov	r0,-(sp)
	jsr	pc,gpasswd
	cmpb	(r0)+,(sp)+
	bne	sorry
	mov	r0,0f
	jsr	r5,compar; 0:..
		br sorry
2:
	clr	r1
2:
	jsr	r5,getc; pbuf
	cmp	r0,$':
	beq	2f
	mpy	$10.,r1
	sub	$'0,r0
	add	r0,r1
	br	2b
2:
	mov	r1,0f
	sys	chown; ttyx; 0:..
	mov	r1,uid
1:
	jsr	r5,getc; pbuf
	cmp	r0,$':
	bne	1b			/ skip ident field
	mov	$dirbuf,r1
1:
	jsr	r5,getc; pbuf
	cmpb	r0,$':
	beq	1f
	movb	r0,(r1)+
	br	1b
1:
	clrb	(r1)
	sys	chdir; dirbuf
	bec	1f
	jsr	r5,mesg; <No directory\n\0>; .even
	br	sorry
1:
	mov	$uname+8.,r1
1:
	tstb	-(r1)
	bne	1f
	movb	$' ,(r1)
	br	1b
1:
	cmpb	ttyx+8.,$'x
	beq	1f
	sys	open; utmp; 1
	bes	1f
	mov	r0,r2
	sys	seek; offset:..; 0
	movb	ttyx+8.,uname+8.
	sys	time
	mov	r0,uname+10.
	mov	r1,uname+12.
	mov	r2,r0
	sys	write; uname; 16.
	mov	r2,r0
	sys	close
1:
	cmpb	ttyx+8.,$'x
	beq	1f
	sys	open; wtmp; 1
	bes	1f
	mov	r0,r1
	sys	seek; 0; 2
	sys	write; uname; 16.
	mov	r1,r0
	sys	close
1:
	jsr	r5,getc; pbuf
	cmp	r0,$'\n
	beq	1f
	mov	$shell,r1
2:
	movb	r0,(r1)+
	jsr	r5,getc; pbuf
	cmp	r0,$'\n
	bne	2b
	clrb	(r1)
1:
	mov	pbuf,r0
	sys	close
	mov	$motd,r0
	jsr	r5,fopen; pbuf
	bes	1f
2:
	jsr	r5,getc; pbuf
	bes	1f
	mov	r0,uname
	mov	$1,r0
	sys	write; uname; 1
	br	2b
1:
	mov	pbuf,r0
	sys	close
	sys	stat; mailf; pbuf
	bes	1f
	tst	pbuf+6
	beq	1f
	jsr	r5,mesg; <You have mail\n\0>; .even
1:
	mov	uid,r0
	sys	setuid
	sys	exec; shell; shellp
	jsr	r5,mesg; <No Shell\n\0>; .even
	sys	exit

gpasswd:
	mov	$passwd,r1
	tstb	(r1)
	bne	3f
	clr	r0
	sys	gtty; ttyb
	bic	$10,ttyb+4		/ turn off echo
	clr	r0
	sys	stty; ttyb
	jsr	r5,mesg; <Password: \0>; .even
2:
	jsr	pc,tgetc
	movb	r0,(r1)+
	beq	1f
	cmp	r1,$passwd+9.
	blo	2b
	dec	r1
	br	2b
1:
	bis	$10,ttyb+4		/ turn on echo
	clr	r0
	sys	stty; ttyb
	jsr	r5,mesg; <\n\0>; .even
3:
	mov	$passwd,r0
	jsr	pc,crypt
	clrb	8(r0)
	rts	pc

guname:
	mov	$uname,r1
	tstb	(r1)
	bne	1f
	clr	(r1)+
	clr	(r1)+
	clr	(r1)+
	clr	(r1)+
	mov	$uname,r1
	jsr	r5,mesg; <Name: \0>; .even
2:
	jsr	pc,tgetc
	movb	r0,(r1)+
	beq	1f
	cmp	r1,$uname+9.
	blo	2b
	dec	r1
	br	2b
1:
	rts	pc

compar:
	mov	(r5)+,r4
1:
	jsr	r5,getc; pbuf
	bes	2f
	cmpb	r0,(r4)+
	beq	1b
	cmp	r0,$':
	bne	1f
	tstb	-(r4)
	bne	1f
	tst	(r5)+
1:
	rts	r5
2:
	tst	(sp)+
	jmp	sorry

tgetc:
	clr	r0
	sys	read; ch; 1
	tst	r0
	bne	1f
	sys	exit
1:
	mov	ch,r0
	cmp	r0,$'\n
	bne	1f
	clr	r0
1:
	rts	pc

shellp:
	mshell
	0
utmp:	</tmp/utmp\0>
wtmp:	</tmp/wtmp\0>
shell:	</bin/sh\0>; .=shell+32.
mshell:	<-\0>
motd:	</etc/motd\0>
mailf:	<mailbox\0>
passwdf:</etc/passwd\0>
ttyx:	</dev/ttyx\0>
.even
.bss
uname: .=.+16.
passwd:	.=.+8.
dirbuf:	.=.+32.
shbuf:	.=.+32.
ttyb:	.=.+6
uid:	.=.+2
ch:	.=.+2
pbuf:	.=.+518.

b
	cmp	r0,$':
	bne	1f
	tstb	-(r4)
	bne	1f
	tst	(r5)+
1:
	rts	r5
2:
	tst	(sp)+
	jmp	sorry

tgetc:
	clr	r0
	sys	read; ch; 1
	tst	r0
	bne	1f
	sys	exit
