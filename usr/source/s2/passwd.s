/ passwd -- change user's password

.globl	mesg
.globl	crypt
.globl	getc
.globl	flush
.globl	fcreat
.globl	putc
.globl	fopen

	cmp	(sp)+,$3
	bge	1f
	jsr	r5,mesg
		<Usage: passwd uid password\n\0>; .even
	sys	exit
1:
	tst	(sp)+
	mov	(sp)+,uidp
	mov	(sp)+,r0
	tstb	(r0)
	beq	1f
	jsr	pc,crypt
	clrb	8(r0)
1:
	mov	r0,cryptp
	mov	$passwf,r0
	jsr	r5,fopen; ibuf
	bec	1f
	jsr	r5,mesg
		<cannot open password file\n\0>; .even
	sys	exit
1:
	sys	stat; tempf; obuf+20.
	bec	2f
	sys	creat; tempf; 222
	bec	1f
2:
	jsr	r5,mesg
		<temp file busy -- try again\n\0>; .even
	sys	exit
1:
	mov	r0,obuf

/ search for uid

comp:
	mov	uidp,r1
1:
	jsr	pc,pcop
	cmp	r0,$':
	beq	1f
	cmpb	r0,(r1)+
	beq	1b
2:
	jsr	pc,pcop
	cmp	r0,$'\n
	bne	2b
	br	comp
1:
	tstb	(r1)+
	bne	2b

/ skip over old password

1:
	jsr	pc,pget
	cmp	r0,$':
	bne	1b

/ copy in new password

	mov	cryptp,r1
1:
	movb	(r1)+,r0
	beq	1f
	jsr	pc,pput
	br	1b
1:
	mov	$':,r0
	jsr	pc,pput

/ validate permission

	clr	r1
1:
	jsr	pc,pcop
	cmp	r0,$':
	beq	1f
	mpy	$10.,r1
	sub	$'0,r0
	add	r0,r1
	br	1b
1:
	sys	getuid
	tst	r0
	beq	1f
	cmp	r0,r1
	beq	1f
	jsr	r5,mesg
		<permission denied\n\0>; .even
	br	done
1:
	inc	sflg
1:
	jsr	pc,pcop
	br	1b

done:
	jsr	r5,flush; obuf
	mov	obuf,r0
	sys	close
	mov	ibuf,r0
	sys	close
	tst	sflg
	beq	1f
	tst	dflg
	bne	1f
	inc	dflg
	mov	$tempf,r0
	jsr	r5,fopen; ibuf
	bec	2f
	jsr	r5,mesg
		<cannot reopen temp file\n\0>; .even
	br	1f
2:
	mov	$passwf,r0
	jsr	r5,fcreat; obuf
	bec	2f
	jsr	r5,mesg
		<cannot reopen password file\n\0>; .even
	br	1f
2:
	jsr	pc,pcop
	br	2b
1:
	sys	unlink; tempf
	sys	exit

pput:
	jsr	r5,putc; obuf
	rts	pc

pget:
	jsr	r5,getc; ibuf
	bes	1f
	rts	pc
1:
	jsr	r5,mesg
		<format error on password file\n\0>; .even
	br	done

pcop:
	jsr	r5,getc; ibuf
	bes	1f
	jsr	r5,putc; obuf
	rts	pc
1:
	tst	sflg
	bne	1f
	jsr	r5,mesg
		<uid not valid\n\0>; .even
1:
	br	done

.data
passwf:	</etc/passwd\0>
tempf:	</tmp/ptmp\0>
.even
.bss
ibuf:	.=.+520.
obuf:	.=.+520.
cryptp:	.=.+2
uidp:	.=.+2
sflg:	.=.+2
dflg:	.=.+2
