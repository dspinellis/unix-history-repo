/ grep -- g/re/p
/
/ grep [ -v ] [ -l ] [ -n ] re [ input ] ...

.globl	mesg, putc, flush, fcreat, _end

	sys	signal; 2; 1
	bit	$1,r0
	bne	1f
	sys	signal; 2; out
1:
	mov	(sp)+,narg
	mov	sp,argp
	jsr	pc,garg
1:
	jsr	pc,garg
	cmpb	(r5),$'-
	bne	1f
	cmpb	1(r5),$'v
	bne	2f
	inc	vflg
	br	1b
2:
	cmpb	1(r5),$'c
	bne	2f
	inc	lflg
	br	1b
2:
	cmpb	1(r5),$'b
	bne	2f
	inc	bflg
	br	1b
2:
	cmpb	1(r5),$'n
	bne	2f
	inc	nflg
	inc	lflg
	br	1b
2:
1:
	jsr	pc,compile

	mov	$1,obuf
	tst	narg
	ble	loop
	jsr	pc,iarg
	br	loop

out:
	inc	nflg
	mov	$2,obuf
	clr	obuf+2
	clr	obuf+4
	clr	obuf+6
	jsr	pc,nline
	jsr	pc,nline
	jsr	pc,pfile
	mov	line,r0
	mov	line+2,r1
	jsr	pc,decml
	jsr	pc,nline
	jsr	r5,flush; obuf
	sys	exit

loop:
	mov	$lbuf,r1
	mov	$elbuf-1,r2
	mov	$'\n,r3
	mov	gc4,r4
	mov	gc5,r5
1:
	dec	r4
	bge	2f

fread:
	mov	gcf,r0
	sys	read; ibuf; 512.
	bes	eloop
	inc	blkno
	mov	r0,r4
	beq	eloop

	mov	$ibuf,r5
	br	1b
2:
	movb	(r5)+,r0
	beq	1b
	cmp	r0,r3		/ nl
	beq	1f
	movb	r0,(r1)+
	cmp	r1,r2		/ elbuf
	blo	1b
	dec	r1
	br	1b
1:
	mov	r5,gc5
	mov	r4,gc4
	add	$1,line+2
	adc	line
	clrb	(r1)+
	jsr	r5,execute
		br 2f
	tst	vflg
	bne	loop
	br	1f
2:
	tst	vflg
	beq	loop
1:
	tst	lflg
	beq	1f
	jsr	pc,pfile
	mov	line,r0
	mov	line+2,r1
	jsr	pc,decml
	mov	$' ,r0
	jsr	r5,putc; obuf
1:
	tst	bflg
	beq	1f
	jsr	pc,pfile
	clr	r0
	mov	blkno,r1
	jsr	pc,decml
	mov	$' ,r0
	jsr	r5,putc; obuf
1:
	mov	$lbuf,r1
1:
	movb	(r1)+,r0
	beq	1f
	jsr	r5,putc; obuf
	br	1b
1:
	jsr	pc,nline
	jsr	r5,flush; obuf
	br	loop

eloop:
	jsr	r5,flush; obuf
	tst	narg
	ble	1f
	jsr	pc,iarg
	br	fread
1:
	sys	exit

iarg:
	mov	r5,-(sp)
	mov	gcf,r0
	beq	1f
	sys	close
1:
	jsr	pc,garg
	mov	r5,fname
	sys	0; 9f
.data
9:
	sys	open; fname: 0; 0
.text
	bec	1f
	mov	fname,r0
	jsr	pc,diag
	mov	$9f,r0
	jsr	pc,diag
.data
9:	<: cannot open\n\0>
	.even
.text
	sys	exit
1:
	mov	r0,gcf
	tst	nflg
	beq	1f
	clr	line
	clr	line+2
1:
	mov	(sp)+,r5
	rts	pc

garg:
	dec	narg
	blt	1f
	mov	*argp,r5
	add	$2,argp
	rts	pc
1:
	mov	$9f,r0
	jsr	pc,diag
.data
9:	<arg count\n\0>
	.even
.text
	sys	exit

compile:
	mov	$ebuf,r3
	movb	(r5)+,r1
	beq	cerr
	cmp	r1,$'^
	beq	1f
	jsr	r5,cop; ecmf
	dec	r5
1:
	cmpb	(r5),$'*
	beq	cerr

cadv:
	movb	(r5)+,r1
	beq	ceof
	cmp	r1,$'\\
	beq	cesc
	cmp	r1,$'.
	beq	cdot
	cmp	r1,$'*
	beq	cast
	cmp	r1,$'$
	beq	cdol
	cmp	r1,$'[
	beq	cccl
	jsr	r5,cop; echr
	mov	r1,(r3)+
	br	cadv

ceof:
	jsr	r5,cop; eeof
	cmp	r3,$eebuf
	blos	1f
	mov	$9f,r0
	jsr	pc,diag
.data
9:	<regular expression too long\n\0>
	.even
.text
	sys	exit
1:
	rts	pc

cesc:
	jsr	r5,cop; echr
	movb	(r5)+,r1
	mov	r1,(r3)+
	bne	cadv

cerr:
	mov	$9f,r0
	jsr	pc,diag
.data
9:	<regular expression syntax\n\0>
	.even
.text
	sys	exit

cdot:
	jsr	r5,cop; edot
	br	cadv

cdol:
	tstb	(r5)
	beq	1f
	jsr	r5,cop; echr
	mov	$'$,(r3)+
	br	cadv
1:
	jsr	r5,cop; edol
	br	cadv

cccl:
	mov	ctab,r0
	add	$32.,r0
	mov	r0,0f
	sys	break; 0:..
	jsr	r5,cop; eccl
	movb	(r5)+,r1
	cmp	r1,$'^
	bne	1f
	mov	$enccl,*f
	movb	(r5)+,r1
1:
	tst	r1
	beq	cerr
	jsr	pc,bitc
	add	ctab,r1
	bisb	r0,(r1)
	movb	(r5)+,r1
	cmp	r1,$']
	bne	1b
	mov	ctab,(r3)+
	mov	0b,ctab
	br	cadv

cast:
	mov	*f,r1
	mov	-(r1),*f
	br	cadv

cop:
	mov	r3,f
	mov	(r5)+,(r3)+
	rts	r5

execute:
	mov	$lbuf,r4
	mov	$ebuf,r3
	jmp	*(r3)+

eeof:
	tst	(r5)+
efail:
	rts	r5

ecmf:
	mov	r3,-(sp)
	mov	r4,-(sp)
	jsr	r5,*(r3)+
		br 1f
	cmp	(sp)+,(sp)+
	br	eeof
1:
	mov	(sp)+,r4
	mov	(sp)+,r3
	tstb	(r4)+
	bne	ecmf
	br	efail

	echrs
echr:
	movb	(r4)+,r1
	cmp	r1,(r3)+
	bne	efail
	jmp	*(r3)+

	echrs
echrs:
	mov	(r3)+,r1
	mov	r4,-(sp)
1:
	cmpb	(r4)+,r1
	beq	1b
	br	east

	edots
edot:
	tstb	(r4)+
	beq	efail
	jmp	*(r3)+

	edots
edots:
	mov	r4,-(sp)
1:
	tstb	(r4)+
	bne	1b
	br	east

	eccls
eccl:
	movb	(r4)+,r1
	beq	efail
	jsr	pc,bitc
	add	(r3)+,r1
	bitb	r0,(r1)
	beq	efail
	jmp	*(r3)+

	enccls
enccl:
	movb	(r4)+,r1
	beq	efail
	jsr	pc,bitc
	add	(r3)+,r1
	bitb	r0,(r1)
	bne	efail
	jmp	*(r3)+

	eccls
eccls:
	mov	r4,-(sp)
	mov	(r3)+,-(sp)
2:
	movb	(r4)+,r1
	beq	1f
	jsr	pc,bitc
	add	(sp),r1
	bitb	r0,(r1)
	bne	2b
1:
	tst	(sp)+
	br	east

	enccls
enccls:
	mov	r4,-(sp)
	mov	(r3)+,-(sp)
2:
	movb	(r4)+,r1
	beq	1f
	jsr	pc,bitc
	add	(sp),r1
	bitb	r0,(r1)
	beq	2b
1:
	tst	(sp)+
	br	east

edol:
	tstb	(r4)
	bne	efail
	jmp	*(r3)+

east:
	dec	r4
	mov	r3,-(sp)
	mov	r4,-(sp)
	jsr	r5,*(r3)+
		br 1f
2:
	add	$6,sp
	br	eeof
1:
	mov	(sp)+,r4
	mov	(sp)+,r3
	cmp	r4,(sp)
	bhi	east
	tst	(sp)+
	br	efail

bitc:
	mov	$1,r0
	mov	r1,-(sp)
	bic	$!7,(sp)
	ash	(sp)+,r0
	ash	$-3,r1
	bic	$!37,r1
	rts	pc

nline:
	mov	$'\n,r0
	jsr	r5,putc; obuf
	rts	pc

pfile:
	tst	nflg
	beq	1f
	mov	fname,r1
	beq	1f
2:
	movb	(r1)+,r0
	beq	2f
	jsr	r5,putc; obuf
	br	2b
2:
	mov	$':,r0
	jsr	r5,putc; obuf
1:
	rts	pc

decml:
	div	$10.,r0
	mov	r1,-(sp)
	mov	r0,r1
	beq	1f
	clr	r0
	jsr	pc,decml
1:
	mov	(sp)+,r0
	add	$'0,r0
	jsr	r5,putc; obuf
	rts	pc

diag:
	mov	r1,-(sp)
	mov	r0,r1
1:
	movb	(r1)+,0f
	beq	1f
	mov	$2,r0
	sys	write; 0f; 1
	br	1b
1:
	mov	(sp)+,r1
	rts	pc

.data
0:	0
.text
.data
ctab:	_end

.bss
narg:	.=.+2
argp:	.=.+2
vflg:	.=.+2
lflg:	.=.+2
nflg:	.=.+2
bflg:	.=.+2
blkno:	.=.+2
line:	.=.+4
f:	.=.+2
ebuf:	.=.+512.; eebuf:
lbuf:	.=.+512.; elbuf:
gcf:	.=.+2
gc4:	.=.+2
gc5:	.=.+2
ibuf:	.=.+512.
obuf:	.=.+518.
