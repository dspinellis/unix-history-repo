/
/

/ roff1 -- text formatter

	.globl ttyn
	nop = 000240
	signal = 48.

ibuf: /init code in ibuf+obuf
	cmp	sp,$end
	bhi	1f
	jsr	r5,string; emes1
	sys	exit
1:
	clr	r0
	jsr	pc,mesg
	sys	signal; 1; place
	sys	signal; 2; place
	jsr	pc,makebf
	sys	open; suffil; 0
	bes	1f
	mov	r0,suff
	sys	seek; 20; 0
	bes	1f
	mov	suff,r0
	sys	read; suftab; 2*26.
1:
	clr	r0
	mov	(sp)+,argc
	dec	argc
	bne	1f
	jmp	place
1:
	tst	(sp)+
	mov	(sp),r4
	cmpb	(r4)+,$'+
	bne	2f
	jsr	r5,pnum; pfrom
	br	3f
2:
	cmpb	-1(r4),$'-
	bne	2f
	cmpb	(r4),$'s
	bne	0f
	inc	stop
	br	3f
0:
	cmpb	(r4),$'h
	bne	0f
	clr	slow
	br	3f
0:
	jsr	r5,pnum; pto
3:
	dec	argc
	bgt	1b
2:
	mov	$nop,get1a
	mov	$ibuf,ibufp
	mov	$ibuf,eibuf
	mov	sp,argp
	jsr	pc,topbot
	clr	r0
1:
	movb	r0,trtab(r0)
	inc	r0
	cmp	r0,$128.
	bne	1b
	jsr	pc,rbreak
	jsr	pc,istop
	jmp	loop
makebf:
	sys	stat; bfn; stbuf
	bec	2f
	sys	creat; bfn; 400
	bec	1f
2:
	incb	bfn+8
	cmpb	bfn+8,$'z
	blos	makebf
	jmp	place
1:
	mov	r0,ibf
	sys	write; sufbuf;128.
	sys	open; bfn;0
	mov	r0,ibf1
	rts	pc

string:
	mov	(r5)+,r1
	mov	r1,r2
	mov	r1,0f
1:
	tstb	(r1)+
	bne	1b
	sub	r2,r1
	mov	r1,1f
	mov	$1,r0
	sys	write; 0:..; 1:..
	rts	r5

emes1: <Too many files.\n\0>
xxx:
.even
obuf=ibuf+512.
.=ibuf+1024.
loop:
	clr	nlflg
	jsr	pc,getchar
	cmpb	r0,cc
	beq	2f
	movb	r0,ch
	jsr	pc,text
	br	loop
2:
	jsr	pc,control
	jsr	pc,flushi
	br	loop

mesg:
	tst	r0
	bne	setsame
	jsr	pc,ttyn
	movb	r0,ttyx+8.
	sys	stat; ttyx; stbuf
	mov	stbuf+4,0f
	mov	0f,1f
	bic	$22,0f
	sys	chmod; ttyx; 0:..
	rts	pc
setsame:
	sys	chmod; ttyx; 1:..
	rts	pc

pnum:
	mov	r4,ibufp
	mov	$37777,eibuf
	jsr	r5,number1; 0
	mov	r0,*(r5)+
	clr	ch
	rts	r5

flushi:
	clr	ch
	tst	nlflg
	bne	1f
	jsr	pc,getchar
	br	flushi
1:
	rts	pc

gettchar:
	tst	ul
	ble	getchar
	tst	ulstate
	beq	3f
	tst	bsc
	bgt	1f
	tst	ulc
	bgt	2f
	clr	ulstate
	br	3f
1:
	dec	bsc
	mov	$010,r0
	rts	pc
2:
	dec	ulc
	mov	$'_,r0
	rts	pc
3:
	jsr	pc,getchar
	cmp	r0,$'0
	blt	1f
	cmp	r0,$'9
	ble	2f
	cmp	r0,$'A
	blt	1f
	cmp	r0,$'Z
	ble	2f
	cmp	r0,$'a
	blt	1f
	cmp	r0,$'z
	ble	2f
1:
	tst	ulc
	bgt	3f
	rts	pc
3:
	mov	$1,ulstate
	mov	r0,ch
	br	gettchar
2:
	inc	bsc
	inc	ulc
	rts	pc

getchar:
	mov	ch,r0
	beq	1f
	clr	ch
	rts	pc
1:
	tst	nlflg
	beq	1f
	mov	$'\n,r0
	rts	pc
1:
	jsr	pc,get1
	cmp	r0,$'\\
	bne	2f
	jsr	pc,get1
	jsr	r5,switch; esctab
	br	3f
2:
	cmp	r0,$033  /prefix
	bne	3f
	jsr	pc,get1
	jsr	r5,switch; pfxtab
3:
	cmp	r0,$'\n
	bne	3f
	inc	nlflg
	clr	column
3:
	mov	r1,-(sp)
	jsr	pc,width
	add	r1,column
	mov	(sp)+,r1
	rts	pc

esctab:
   .byte 'd, 032  /hlf (down)
   .byte 'u, 035  /hlr (up)
   .byte 'r, 036  /flr (reverse)
   .byte 'x, 016  /SO (extra chars)
   .byte 'y, 017  /SI (normal characters)
   .byte 'l, 0177 /delete
   .byte 't, 011  /hor tab
   .byte 'a, 0100 /at sign
   .byte 'n, 043  /number sign
   .byte '\\, 134 /backslash
   .byte 0, 0

pfxtab:
   .byte '7, 036  /flr
   .byte '8, 035  /hlr
   .byte '9, 032  /hlf
   .byte '4, 030  /brs
   .byte '3, 031  /rrs
   .byte '1, 026  /set hor tabs
   .byte '2, 027  /clr hor tabs
   .byte 0,0
pfxtab1:

switch:
	mov	r1,-(sp)
	mov	(r5)+,r1
1:
	cmpb	(r1)+,r0
	beq	1f
	tstb	(r1)+
	bne	1b
	cmp	r1,$pfxtab
	ble	0f
	cmp	r1,$pfxtab1
	bgt	0f
	mov	$037,r0
0:
	mov	(sp)+,r1
	rts	r5
1:
	movb	(r1)+,r0
	mov	(sp)+,r1
	rts	r5

get1:
	tst	nspace
	ble	1f
	dec	nspace
	mov	tabc,r0
	rts	pc
1:
	mov	r1,-(sp)
4:
	tst	ip
	beq	5f
	jsr	pc,rbf
	br	6f
5:
	tst	nx
	bne	0f
	mov	ibufp,r1
	cmp	r1,eibuf
	bne	3f
0:
	mov	ifile,r0
	bne	2f
1:
	jsr	pc,nextfile
2:
	clr	nx
	sys	read; ibuf; 512.
	bes	done
	tst	r0
	beq	1b
	mov	$ibuf,r1
	add	r1,r0
	mov	r0,eibuf
3:
	movb	(r1)+,r0
	mov	r1,ibufp
1:
	cmp	r0,$011	/tab
	bne	6f
	mov	(sp)+,r1
	mov	$tabtab,r0
	inc	nspace
1:
	tstb	(r0)
	beq	get1
	cmpb	column,(r0)+
	bge	1b
	movb	-(r0),nspace
	sub	column,nspace
	br	get1
6:
get1a:	br	7f
	tst	r0
	beq	4b
7:
	mov	(sp)+,r1
	rts	pc

nextfile:
	mov	ifile,r0
	beq	1f
	sys	close
1:
	tst	nx
	beq	2f
	mov	$nextf,0f
	br	3f
2:
	dec	argc
	blt	done
	mov	*argp,0f
	add	$2,argp
3:
	sys	open; 0:..; 0
	bes	done
	mov	r0,ifile
	rts	pc

done:
	jsr	pc,rbreak
	jsr	pc,eject
	jsr	pc,flush
place:
	sys	signal; 2; 1
	mov	$1,r0
	jsr	pc,mesg
	sys	unlink; bfn
	sys	exit

	rts	pc

putchar:
	cmp	pn,pfrom
	blt	2f
	clr	pfrom
	bic	$!177,r0
	beq	2f
	movb	trtab(r0),r0
	cmp	r0,$' 
	bne	1f
	inc	nsp
2:
	rts	pc
1:
	cmp	r0,$'\n
	bne	1f
	clr	nsp
	clr	ocol
	br	2f
1:
	tst	nsp
	beq	2f
	tst	slow
	bne	4f
	jsr	pc,dsp
	cmp	nsp,r1
	blt	4f
	mov	$011,3f+2
	cmp	r1,$1
	bgt	8f
	mov	$040,3f+2
	dec	nsp
	br	9f
8:
	sub	r1,nsp
9:
	mov	r0,-(sp)
3:
	mov	$011,r0
	jsr	pc,pchar1
	mov	(sp)+,r0
	br	1b
4:
	mov	r0,-(sp)
	mov	$' ,r0
	jsr	pc,pchar1
	mov	(sp)+,r0
	dec	nsp
	bne	4b
2:
	cmp	r0,$026
	blt	2f
	cmp	r0,$037
	beq	3f
	bgt	2f
	mov	r0,-(sp)
	jsr	r5, switch; unpfx
	cmp	(sp)+,r0
	beq	2f
	mov	r0,-(sp)
	mov	$033,r0  /prefix
	jsr	pc,pchar1
	dec	ocol
	mov	(sp)+,r0
2:
pchar1:
	cmp	r0,$011
	bne	1f
	jsr	pc,dsp
	br	2f
1:
	jsr	pc,width
2:
	add	r1,ocol
	movb	r0,*obufp
	inc	obufp
	cmp	obufp,$obuf+128.
	beq	flush
3:
	rts	pc

dsp:
	clr	r1
1:
	add	$8.,r1
	cmp	ocol,r1
	bgt	1b
	sub	ocol,r1
	bne	2f
	mov	$8.,r1
2:
	rts	pc


unpfx:
   .byte 032, '9
   .byte 035, '8
   .byte 036, '7
   .byte 031, '3
   .byte 030, '4
   .byte 026, '1
   .byte 027, '2
   .byte 0,0

flush:
	mov	obufp,r0
	sub	$obuf,r0
	mov	r0,0f
	mov	$1,r0
	sys	write; obuf; 0:0
	mov	$obuf,obufp
	rts	pc

control:
	jsr	pc,getchar
	mov	r0,-(sp)
	jsr	pc,getchar
	swab	r0
	bis	(sp),r0
	mov	$contab,r1
1:
	mov	(r1)+,(sp)
	bic	$100000,(sp)
	cmp	r0,(sp)
	bne	4f
	mov	(r1),(sp)
	tst	-(r1)
	bpl	3f
	jsr	pc,flushi
	cmp	ilistp,$iliste
	bgt	5f
	mov	ip,*ilistp
	add	$2,ilistp
	mov	(sp),ip
	br	5f
3:
	jmp	*(sp)+
4:
	cmp	(r1)+,$-1
	bne	1b
5:
	tst	(sp)+
	rts	pc

contab:
   <ad>; casead
   <bp>; casebp
   <br>; casebr
   <cc>; casecc
   <ce>; casece
   <ds>; caseds
   <fi>; casefi
   <in>; casein
   <ix>; caseix
   <li>; caseli
   <ll>; casell
   <ls>; casels
   <na>; casena
   <ne>; casene
   <nf>; casenf
   <pa>; casepa
   <bl>; casebl
   <pl>; casepl
   <sk>; casesk
   <sp>; casesp
   <ss>; casess
   <ta>; caseta
   <ti>; caseti
   <tr>; casetr
   <ul>; caseul
   <un>; caseun
   <he>; casehe
   <hx>; casehx
   <fo>; casefo
   <eh>; caseeh
   <oh>; caseoh
   <ef>; caseef
   <of>; caseof
   <m1>; casem1
   <m2>; casem2
   <m3>; casem3
   <m4>; casem4
   <hc>; casehc
   <hy>; casehy
   <n1>; casen1
   <n2>; casen2
   <nn>; casenn
   <ni>; caseni
   <jo>; casejo
   <ar>; casear
   <ro>; casero
   <nx>; casenx
   <po>; casepo
   <de>; casede
   <ig>; caseig
   <tc>; casetc
   <mk>; casemk
bnames: .=.+100.
    -1; -1
