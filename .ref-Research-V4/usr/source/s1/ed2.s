/
/ copyright 1972 bell telephone laboratories inc.
/

/ ed2 -- text editor

address:
	clr	minflg
	clr	addr

ad1:
	jsr	r5,switch; addrt
	tst	minflg
	bne	9f
	tst	addr
	beq	1f
	tst	(r5)+
1:
	rts	r5

addrt:
	' ;	ad1
	11;	ad1
	'+;	ad1
	'/;	fsrch
	'?;	bsrch
	'-;	amin
	'.;	adot
	'$;	adol
	'0;	num0
	'1;	num1
	'2;	num1
	'3;	num1
	'4;	num1
	'5;	num1
	'6;	num1
	'7;	num1
	'8;	num1
	'9;	num1
	'';	amark
	'^;	up1
	0

adot:
	jsr	r5,addrel; dot
	br	ad1

adol:
	jsr	r5,addrel; dol
	br	ad1

up1:
	mov	dot,r1
	cmp	r1,zero
	beq	9f
	tst	-(r1)
	mov	r1,f
	jsr	r5,addrel; f
	br	ad1

9:	jmp	error

fsrch:
	jsr	r5,compile
	mov	dot,-(sp)
1:
	add	$2,(sp)
	cmp	(sp),dol
	blos	2f
	mov	zero,(sp)
2:
	cmp	(sp),zero
	beq	2f
	mov	(sp),r4
	jsr	r5,execute
		br  2f
	br	3f
2:
	cmp	(sp),dot
	bne	1b
	br	9b

bsrch:
	jsr	r5,compile
	mov	dot,-(sp)
1:
	sub	$2,(sp)
	cmp	(sp),zero
	bhis	2f
	mov	dol,(sp)
2:
	cmp	(sp),zero
	beq	2f
	mov	(sp),r4
	jsr	r5,execute
		br  2f
	br	3f
2:
	cmp	(sp),dot
	bne	1b
	br	9b
3:
	mov	(sp)+,f
	jsr	r5,addrel; f
	br	ad1

addrel:
	add	minflg,addr
	bne	9b
	mov	*(r5)+,addr
	rts	r5

amin:
	tst	minflg
	bne	9f
	inc	minflg
	jmp	ad1

num0:
	mov	$8.,-(sp)
	br	1f

num1:
	mov	$10.,-(sp)
1:
	mov	r1,peekc
	clr	r3
1:
	jsr	r5,getc
	cmp	r1,$'0
	blt	1f
	cmp	r1,$'9
	bgt	1f
	mpy	(sp),r3
	sub	$'0,r1
	add	r1,r3
	br	1b
1:
	mov	r1,peekc
	tst	addr
	bne	1f
	mov	zero,addr
1:
	asl	r3
	tst	minflg
	beq	1f
	clr	minflg
	neg	r3
1:
	add	r3,addr
	jmp	ad1

9:	jmp	error
8:	jmp	advanc

amark:
	jsr	r5,getc
	jsr	r5,findk
	bne	9b
	mov	[kaddr-kname](r2),r2
	mov	zero,r4
1:
	mov	(r4)+,r3
	bic	$1,r3
	cmp	r2,r3
	beq	2f
	cmp	r4,dol
	blos	1b
	br	9b
2:
	tst	-(r4)
	mov	r4,f
	jsr	r5,addrel; f
	jmp	ad1

findk:
	clr	-(sp)
	mov	$kname,r2
1:
	tst	(r2)
	beq	2f
	cmp	r1,(r2)
	beq	3f
	tst	(r2)+
	cmp	r2,$ekname
	blos	1b
	clr	r2
2:
	inc	(sp)
3:
	tst	(sp)+
	rts	r5

findka:
	mov	$kaddr,r1
1:
	cmp	(r1)+,r0
	beq	2f
	cmp	r1,$ekaddr
	blos	1b
	clr	r1
	rts	r5
2:
	tst	-(r1)
	rts	r5

printn:
	mov	r0,-(sp)
	mov	r1,-(sp)
	clr	r0
	mov	count,r1
	mov	$linebuf,r3
	jsr	r5,1f
	movb	$'\n,(r3)+
	jsr	r5,print; linebuf
	mov	(sp)+,r1
	mov	(sp)+,r0
	rts	r5

1:
	clr	r0
	dvd	$10.,r0
	mov	r1,-(sp)
	mov	r0,r1
	beq	1f
	jsr	r5,1b
1:
	add	$'0,(sp)
	movb	(sp)+,(r3)+
	rts	r5

set.:
	tst	adrflg
	bne	testa
	br	1f

set1d:
	tst	adrflg
	bne	testa
	mov	zero,r4
	cmp	r4,dol
	bhis	9b
	tst	(r4)+
	mov	r4,addr1
1:
	mov	dol,addr2
	rts	r5

set.d:
	tst	adrflg
	bne	testa
	mov	dot,r4
	mov	r4,addr1
	mov	r4,addr2
	rts	r5

testa:
	mov	addr1,r4
	cmp	r4,addr2
	bhi	9b
	jsr	r5,1f
	mov	addr2,r4
	jsr	r5,1f
	rts	r5

1:
	cmp	r4,zero
	blo	9f
	cmp	r4,dol
	bhi	9f
	rts	r5

nonzero:
	cmp	addr1,zero
	blos	9f
	rts	r5

setna:
	tst	adrflg
	bne	9f
	rts	r5

8:	jmp	advanc
9:	jmp	error

newline:
	jsr	r5,getc
	cmp	r1,$'p
	bne	1f
	jsr	r5,getc
	inc	pflag
1:
	cmp	r1,$'\n
	bne	9b
	rts	r5

rdline:
	mov	$linebuf,r4
1:
	jsr	r5,getc
	movb	r1,(r4)+
	cmp	r1,$'\n
	beq	1f
	cmp	r4,$elinbuf
	blo	1b
	br	9b
1:
	cmp	linebuf,$".\n 
	beq	8b
	rts	r5

append:
	cmp	dol,$ebuffer-5
	bhis	9b
	jsr	r5,*(r5)+
	mov	dol,r4
	tst	(r4)+
	mov	r4,dol
	jsr	r5,setbrk
	mov	r4,r3
	tst	(r3)+
	clr	(r3)
	add	$2,dot
1:
	mov	-(r4),-(r3)
	cmp	r4,dot
	bhi	1b
	mov	$linebuf,r0
	jsr	r5,putline
	rts	r5

putline:
	mov	r0,r4
	mov	oblkp,r2
	bit	$1,r2
	beq	2f
	jsr	r5,1f
2:
	mov	dska,r1
	add	r2,r1
	mov	r1,*dot
2:
	movb	(r4)+,r1
	clr	r0
	jsr	r5,1f
	cmp	r1,$'\n
	bne	2b
	mov	r2,oblkp
	rts	r5

1:
	movb	r1,obuf(r2)
	inc	r2
	bit	$777,r2
	bne	2f
	mov	fout,r0
	sys	0;7f
.data
7:
	sys	seek; dska: 0; 0
.text
	mov	fout,r0
	sys	write; obuf; 512.
	bes	tmperr
	cmp	dska,$65536.-512.
	bhis	tmperr
	add	$512.,dska
	clr	r2
2:
	rts	r5

tmperr:
	mov	$1,r0
	sys	write; scream; 3
	br	9f

scream:	<TMP>; .even

getline:
	mov	r3,-(sp)
	mov	$linebuf,r1
	mov	(r4),r4
	bic	$1,r4
	mov	r4,r3
	bic	$777,r4
1:
	mov	$ibuf,2f+2
	bic	$!777,r3
	cmp	r4,iblk
	beq	4f
	cmp	r4,dska
	bne	3f
	mov	$obuf,2f+2
	br	4f
3:
	mov	r4,iblk
	mov	fin,r0
	sys	0; 7f
.data
7:
	sys	seek; iblk: -1; 0
.text
	mov	fin,r0
	sys	read; ibuf; 512.
4:
	jmp	2f
.data
2:
	movb	ibuf(r3),r0
	jmp	7f
.text
7:
	movb	r0,(r1)+
	cmp	r0,$'\n
	beq	1f
	inc	r3
	bit	$777,r3
	bne	4b
	add	$512.,r4
	br	1b
1:
	mov	(sp)+,r3
	rts	r5

9:	jmp	error

filnam:
	mov	$filebuf,r4
	jsr	r5,getc
	cmp	r1,$'\n
	beq	2f
	cmp	r1,$' /
	bne	9b
1:
	jsr	r5,getc
	cmp	r1,$' /
	beq	1b
	cmp	r1,$'\n
	beq	2f
	movb	r1,(r4)+
	cmp	r4,$filebuf+filsiz
	blo	1b
	br	9b
2:
	cmp	r4,$filebuf
	beq	4f
	clrb	(r4)
	tst	eflag
	beq	3f
	clr	eflag
1:
	mov	$filebuf,r3
	mov	$filsav,r4
2:
	movb	(r3)+,(r4)+
	bne	2b
3:
	tstb	filsav
	beq	1b
	rts	r5
4:
	tstb	filsav
	beq	9b
	mov	$filsav,r3
	mov	$filebuf,r4
	br	2b

rdfile:
	mov	$linebuf,r4
1:
	jsr	r5,fchar
	movb	r1,(r4)+
	cmp	r1,$'\n
	beq	1f
	cmp	r4,$elinbuf
	blo	1b
	br	9b
1:
	rts	r5

fchar:
	dec	filec
	blt	1f
	movb	*filep,r1
	inc	filep
	tst	r1
	beq	fchar
	rts	r5
1:
	mov	f,r0
	sys	read; fbuf; efbuf-fbuf
	bes	9b
	add	r0,count
	mov	r0,filec
	beq	1f
	mov	$fbuf,filep
	br	fchar
1:
	mov	f,r0
	sys	close
	tst	nonum
	bne	0f
	jsr	r5,printn
0:
	jmp	advanc

delete:
	jsr	r5,nonzero
	mov	addr1,r3
	mov	dol,r0
	add	r3,r0
	mov	addr2,r4
	sub	r4,r0
	sub	$2,r0
	mov	r0,dol
	tst	(r4)+
1:
	mov	(r4)+,(r3)+
	bne	1b
	jsr	r5,setbrk
	rts	r5

inite:
	movb	$'a,qetmp+8
1:
	sys	stat; qetmp; linebuf
	bec	2f
	sys	creat; qetmp; 400
	bec	1f
2:
	incb	qetmp+8
	cmpb	qetmp+8,$'z
	blos	1b
	br	e1
1:
	mov	r0,fout
	sys	open; qetmp; 0
	bes	e1
	mov	r0,fin
	mov	$buffer,r0
	mov	r0,zero
	mov	r0,dol
	mov	r0,dot
	jsr	r5,setbrk
	mov	$-1,iblk
	mov	$1,oblkp
	clr	dska
	mov	$kname,r0
	mov	r0,kp
1:
	clr	(r0)+
	cmp	r0,$ekname
	blos	1b
	rts	r5

e1:
	jsr	r5,print; qbadf
	sys	exit

terme:
	mov	fin,r0
	sys	close
	mov	fout,r0
	sys	close
	sys	unlink; qetmp
	rts	r5

setbrk:
	mov	dol,-(sp)
	add	$516.,(sp)
	bic	$777,(sp)
	cmp	(sp),brk
	beq	1f
	mov	(sp),brk
	sys	0; 7f
.data
7:
	sys	break; brk:..
.text
1:
	tst	(sp)+
	rts	r5

