/
/ copyright 1972 bell telephone laboratories inc.
/

/ ed1 -- text editor


signal = 48.

	cmpb	*2(sp),$'-
	bne	1f
	inc	shflg
	jsr	r5,print; qed
1:
	jsr	r5,inite
	sys	signal; 1; 1
	ror	r0
	bcs	1f
	sys	signal; 1; cq1		/ hangup
1:
	sys	signal; 2; 1
	bcs	1f
	sys	signal; 2; error	/ interrupt
	inc	intrflg
1:
	sys	signal; 3; 1
	clr	filebuf
	clr	filsav
	clr	exprbuf
	mov	$'\n,nl
	mov	sp,gsp
	mov	sp,r4
	mov	(r4)+,argc
	dec	argc
	ble	8f
	tst	(r4)+
0:
	mov	(r4)+,r3
	cmpb	(r3),$'-
	bne	1f
	inc	nonum
	dec	argc
	ble	8f
	br	0b
1:
	mov	$filebuf,r1
	mov	$filsav,r2
1:
	movb	(r3),(r2)+
	movb	(r3)+,(r1)+
	bne	1b
	mov	dol,addr2
	mov	$12,ch
	jmp	cr1
8:	jmp	advanc
9:
error:
	jsr	r5,print; qerr
	sys	signal; 2; error	/ interrupt
	clr	r0
	sys	seek; 0; 2
	clr	pflag
	jsr	r5,gclear
	mov	ch,peekc
1:
	jsr	r5,getc
	cmp	r1,$12
	bne	1b

8:
advanc:
	mov	gsp,sp
	tst	pflag
	beq	1f
	clr	pflag
	mov	dot,addr1
	mov	dot,addr2
	jmp	cp1
1:
	tst	gflag
	beq	1f
	tstb	*gbufp
	bne	3f
	mov	zero,r4
2:
	tst	(r4)+
	cmp	r4,dol
	bhi	2f
	bit	$1,(r4)
	beq	2b
	dec	(r4)
	mov	r4,dot
	mov	$gbuf,gbufp
	br	3f
2:
	jsr	r5,gclear
1:
	tst	shflg
	beq	3f
	mov	$1,r0
	sys	write; prompt; 1
3:
	clr	adrflg
	jsr	r5,address
		br command
	inc	adrflg
	mov	addr,addr1

adrloop:
	mov	addr,addr2
	jsr	r5,switch; delimt
	br	command

delimt:
	';;	1f
	',;	2f
	0

1:
	cmp	addr,dol
	bhi	9b
	mov	addr,dot
2:
	jsr	r5,address
		br 9b
	mov	addr2,addr1
	br	adrloop

command:
	jsr	r5,switch; comndt
	br	9b

comndt:
	'a;	ca
	'c;	cc
	'd;	cd
	'e;	ce
	'f;	cf
	'g;	cg
	'i;	ci
	'k;	ck
	'l;	cl
	'm;	cm
	'n;	cn
	'p;	cp
	'o;	co
	'q;	cq
	'r;	cr
	's;	cs
/	't;	ct
	'v;	cv
	'w;	cw
	'=;	ceq
	'!;	cex
	'\n;	cnl
	0

co:
	jsr	r5,setna
	jsr	r5,getc
	cmp	r1,$'\n
	beq	9f
	mov	r1,r3
	jsr	r5,newline
	cmp	r3,$'s
	bne	0f
	inc	nonum
	br	1f
0:
	cmp	r3,$'v
	bne	9f
	clr	nonum
1:
	jmp	advanc

ca:
	jsr	r5,set.d
	jsr	r5,newline
	br	ci2

cc:
	jsr	r5,set.d
	jsr	r5,newline
	jsr	r5,delete
	mov	addr1,addr2
	br	ci1

cd:
	jsr	r5,set.d
	jsr	r5,newline
	jsr	r5,delete
	mov	addr1,dot
	cmp	dot,dol
	blos	8f
	mov	dol,dot
	br	8f

ci:
	jsr	r5,set.d
	jsr	r5,newline
ci1:
	sub	$2,addr2
	cmp	addr2,zero
	blo	9f
ci2:
	mov	addr2,dot
1:
	jsr	r5,append; rdline
	br	1b

8:	jmp  advanc
9:	jmp  error

cp:
	jsr	r5,set.d
	jsr	r5,newline
cp1:
	jsr	r5,nonzero
1:
	mov	addr1,r4
	jsr	r5,getline
	jsr	r5,print; linebuf
	add	$2,addr1
	cmp	addr1,addr2
	blos	1b
	mov	addr2,dot
	br	8b

cq:
	jsr	r5,setna
	jsr	r5,newline

cq1:
	jsr	r5,terme
	sys	exit

ce:
	jsr	r5,setna
	jsr	r5,getc
	cmp	r1,$' /
	bne	9b
	mov	$fout+2,r0
1:
	clr	(r0)+
	cmp	r0,brk
	bne	1b
	jsr	r5,terme
	jsr	r5,inite
	mov	$' ,peekc
	mov	pc,eflag

cr:
	jsr	r5,set.
	jsr	r5,filnam
cr1:
	sys	open; filebuf; 0
	bes	9b
	mov	r0,f
	mov	addr2,dot
	clr	filec
	clr	count
1:
	jsr	r5,append; rdfile
	br	1b

cs:
	jsr	r5,set.d
	jsr	r5,nonzero
1:
	jsr	r5,getc
	cmp	r1,$' 
	beq	1b
	cmp	r1,$'\n
	beq	9b
	mov	r1,-(sp)
	jsr	r5,compile
	mov	$rhsbuf,r3
1:
	jsr	r5,getc
	cmp	r1,$'\\
	bne	2f
	jsr	r5,getc
	cmp	r1,$'\n
	beq	9b
	bis	$200,r1
2:
	movb	r1,(r3)+
	cmp	r1,$'\n
	beq	9b
	cmp	r1,(sp)
	bne	1b
	clrb	-(r3)
	jsr	r5,getc
	cmp	r1,$'g
	bne	1f
	inc	gsubf
	br	2f
1:
	clr	gsubf
	mov	r1,peekc
2:
	jsr	r5,newline
	mov	gflag,(sp)
1:
	mov	addr1,r4
	jsr	r5,execute
		br 4f
	mov	pc,(sp)
	jsr	r5,dosub
	tst	gsubf
	beq	2f
3:
	cmpb	*loc2,$'\n
	beq	2f
	jsr	r5,gexecute
		br 2f
	jsr	r5,dosub
	br	3b
2:
	mov	$subbuf,r0
	mov	*addr1,-(sp)
	jsr	r5,putline
	mov	(sp)+,r0
0:
	jsr	r5,findka
	beq	4f
	mov	*dot,(r1)
	br	0b
4:
	add	$2,addr1
	cmp	addr1,addr2
	blos	1b
	tst	(sp)+
	beq	9f
	br	8f

8:	jmp  advanc
9:	jmp  error

cw:
	jsr	r5,set1d
	jsr	r5,nonzero
	jsr	r5,filnam
	sys	creat; filebuf; 666
	bes	9b
	mov	r0,f
	clr	count
	mov	$fbuf,filep
1:
	mov	addr1,r4
	jsr	r5,getline
	mov	$linebuf,r0
	mov	r0,r1
	jsr	r5,size
	add	r4,count
2:
	movb	(r1)+,r4
	movb	r4,*filep
	inc	filep
	cmp	filep,$efbuf
	blo	3f
	mov	f,r0
	sys	write; fbuf; efbuf-fbuf
	bes	9b
	mov	$fbuf,filep
3:
	cmp	r4,$'\n
	bne	2b
	add	$2,addr1
	cmp	addr1,addr2
	blos	1b
	mov	filep,r0
	sub	$fbuf,r0
	beq	1f
	mov	r0,0f
	mov	f,r0
	sys	0; 7f
.data
7:
	sys	write; fbuf; 0:..
.text
	bes	9b
1:
	mov	f,r0
	sys	close
	tst	nonum
	bne	8b
	jsr	r5,printn
	br	8b

ceq:
	jsr	r5,set1d
	jsr	r5,newline
	mov	addr2,count
	sub	zero,count
	asr	count
	jsr	r5,printn
	br	8b

cex:
	jsr	r5,setna
	sys	fork
		br 1f
	bes	9b
	mov	r0,-(sp)
	sys	signal; 2; 1		/ ignore intr
2:
	sys	wait
	bes	2f
	cmp	r0,(sp)
	bne	2b
2:
	tst	(sp)+
	tst	intrflg
	beq	3f
	sys	signal; 2; error	/ reinstate intr
3:
	jsr	r5,print; qex
	br	8b
1:
	mov	fin,r0
	sys	close
	mov	fout,r0
	sys	close
	sys	exec; 2f; 1f
	sys	exit
1:
	2f
	3f
	0
2:	</bin/sh\0>
3:	<-t\0>
	.even

9:	jmp	error
cnl:
	jsr	r5,set.d
	tst	adrflg
	bne	1f
	cmp	dot,dol
	bhis	9b
	add	$2,addr2
1:
	mov	addr2,addr1
	jmp	cp1

cl:
	jsr	r5,set.d
	jsr	r5,newline
	jsr	r5,nonzero
1:
	mov	addr1,r4
	jsr	r5,getline
	jsr	r5,cprint
	add	$2,addr1
	cmp	addr1,addr2
	blos	1b
	mov	addr2,dot
	jmp	8f

cprint:
	mov	$linebuf,r3
1:
	mov	$subbuf,r4
	clr	count
2:
	inc	count
	cmp	count,$50.
	blos	3f
	movb	$'\\,(r4)+
	movb	$'\n,(r4)+
	jsr	r5,print; subbuf
	br	1b
3:
	movb	(r3)+,r0
	cmp	r0,$33
	bne	3f
	movb	(r3)+,r0
	cmp	r0,$'\n
	beq	1f
	movb	r0,(r4)+
	movb	$10,(r4)+
	movb	$'^,(r4)+
	br	2b
3:
	cmp	r0,$'\n
	beq	1f
	mov	$estab,r1
3:
	cmpb	r0,(r1)+
	beq	3f
	tstb	(r1)+
	bne	3b
	cmpb	r0,$040
	bhis	0f
	movb	$'\\,(r4)+
	jsr	pc,numb
	add	$3,count
	br	2b
0:
	movb	r0,(r4)+
	br	2b
3:
	movb	(r1),(r4)+
	movb	$10,(r4)+
	movb	$'-,(r4)+
	br	2b
1:
	movb	$'\n,(r4)+
	jsr	r5,print; subbuf
	rts	r5

numb:
	mov	r2,-(sp)
	mov	r3,-(sp)
	mov	r0,r3
	mov	$3,r0
	jsr	pc,numb1
	mov	(sp)+,r3
	mov	(sp)+,r2
	rts	pc
numb1:
	clr	r2
	dvd	$8.,r2
	mov	r3,-(sp)
	mov	r2,r3
	dec	r0
	beq	1f
	jsr	pc,numb1
1:
	add	$'0,(sp)
	movb	(sp)+,(r4)+
	rts	pc

estab:
	.byte	10,'\\
	.byte	11,'>
	.byte	15,'<
	.byte	16,'O
	.byte	17,'I
	.byte	00,00

8:	jmp advanc
9:	jmp error

cg:
	clr	-(sp)
	br	1f
cv:
	mov	pc,-(sp)
1:
	jsr	r5,set1d
	jsr	r5,nonzero
	tst	gflag
	bne	9b
1:
	jsr	r5,getc
	cmp	r1,$' /
	beq	1b
	cmp	r1,$'\n
	beq	9b
	jsr	r5,compile
	mov	addr1,-(sp)
	mov	zero,r4
1:
	tst	(r4)+
	cmp	r4,dol
	bhi	1f
	bic	$1,(r4)
	br	1b
1:
	mov	(sp),r4
	cmp	r4,addr2
	bhi	1f
	add	$2,(sp)
	jsr	r5,execute
		br 2f
	tst	2(sp)
	bne	1b
	mov	(sp),r4
	inc	-(r4)
	br	1b
2:
	tst	2(sp)
	beq	1b
	mov	(sp),r4
	inc	-(r4)
	br	1b
1:
	mov	$gbuf,r4
	cmp	(sp)+,(sp)+
	clr	r3
1:
	jsr	r5,getc
	cmp	r1,$'\n
	bne	2f
	tst	r3
	beq	5f
	tstb	-(r4)
	br	3f
2:
	cmp	r1,$'\\
	bne	3f
	inc	r3
	br	4f
3:
	clr	r3
4:
	movb	r1,(r4)+
	cmp	r4,$egbuf
	bhis	9f
	br	1b
5:
	movb	r1,(r4)+
	clrb	(r4)
	inc	gflag

8:	jmp	advanc
9:	jmp	error

cm:
	jsr	r5,set.d
	jsr	r5,nonzero
	jsr	r5,address
		br 9b
	jsr	r5,newline
	cmp	addr,addr1
	blo	1f
	cmp	addr,addr2
	bhi	2f
	br	9b
1:
	mov	addr,r1
	tst	(r1)+
	mov	addr2,addr
	mov	addr1,r2
	mov	addr2,dot
	sub	r2,dot
	add	r1,dot
1:
	cmp	r2,r1
	beq	8b
	tst	-(r2)
	br	3f
2:
	mov	addr1,r1
	mov	addr2,r2
	mov	addr,dot
3:
	mov	r2,r3
	tst	(r3)+
	mov	r3,r4
	tst	(r4)+
	mov	(r3),r0
4:
	mov	-(r3),-(r4)
	cmp	r3,r1
	bhi	4b
	mov	r0,(r3)
	tst	(r1)+
	tst	(r2)+
	cmp	r2,addr
	blo	3b
	br	8b

gclear:
	clr	gflag
	mov	$gbuf,gbufp
	dec	gbufp
	clrb	*gbufp
	rts	r5

ck:
	jsr	r5,set.d
	jsr	r5,getc
	cmp	r1,$040
	blos	9b
	mov	r1,r3
	jsr	r5,newline
	mov	r3,r1
	jsr	r5,findk
	beq	1f
	mov	kp,r2
	add	$2,kp
	cmp	kp,$ekname
	blo	1f
	mov	$kname,kp
1:
	mov	r1,(r2)
	mov	addr2,dot
	mov	*addr2,[kaddr-kname](r2)
	bic	$1,[kaddr-kname](r2)
	br	8b

cn:
	jsr	r5,setna
	jsr	r5,newline
	mov	$kname,r1
	mov	$linebuf,r0
1:
	movb	(r1)+,(r0)+
	beq	1f
	tstb	(r1)+
	br	1b
1:
	movb	$'\n,-(r0)
	jsr	r5,print; linebuf
	br	8f

cf:
	jsr	r5,setna
	jsr	r5,getc
	cmp	r1,$'\n
	beq	1f
	mov	r1,peekc
	mov	pc,eflag
	jsr	r5,filnam
1:
	mov	$filsav,r1
1:
	tstb	(r1)+
	bne	1b
	movb	$'\n,-(r1)
	jsr	r5,print; filsav
	clrb	(r1)

8:	jmp	advanc
/ct:
/	jsr	r5,set.d
/	jsr	r5,nonzero
/	jsr	r5,address
/		br 4f
/	jsr	r5,newline
/	cmp	addr,addr1
/	blo	1f
/	cmp	addr,addr2
/	bhi	2f
/4:
/	jmp	9b
/1:
/	mov	$2,gork
/	br	3f
/2:
/	clr	gork
/3:
/	mov	addr1,r4
/	jsr	r5,append;	getline
/	add	$2,addr1
/	cmp	addr1,addr2
/	bhi	8b
/	add	gork,addr1
/	add	gork,addr2
/	br	3b
/.bss
/gork:	.=.+2
/.text
