/
/

/ nroff1 -- text formatter

/	rts = 104000
	signal = 48.
	nop = 000240

	jmp	ibuf
.data
ibuf: /init code in ibuf+obuf
	cmp	sp,$end
	bhi	1f
	jsr	r5,string; emes1
	sys	exit
1:
	clr	r0
	jsr	pc,ttyn
	cmpb	r0,$'x
	bne	0f
	inc	iflg
	mov	$1,r0
	jsr	pc,ttyn
0:
	movb	r0,ttyx+8
	clr	r0
	jsr	pc,mesg
	sys	open; ttyx; 2
	bes	0f
	mov	r0,ttyid
	mov	r0,ttyod
0:
	mov	ttyid,r0
	sys	gtty; ttys
	sys	signal; 1; place
	sys	signal; 2; place
	bit	$1,r0
	beq	0f
	sys	signal; 2; 1	/no deletes
	sys	signal; 3; 1	/no quits
	sys	signal; 1; 1	/allow hangup
0:
	mov	$'%,rlist
	mov	$"nl,rlist+2
	mov	$"dn,rlist+4
	mov	$"yr,rlist+6
	mov	$"mo,rlist+8.
	mov	$"dy,rlist+10.
	mov	$"dw,rlist+12.
	mov	$"hp,rlist+14.
	mov	sp,r0
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
	ble	4f
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
	tstb	(r4)
	beq	4f
	cmpb	(r4),$'m
	bne	0f
	mov	$nextf1,r0
	tstb	(r4)+
8:
	movb	(r4)+,(r0)+
	bne	8b
	inc	nx
	inc	mflg
	br	3f
0:
	cmpb	(r4),$'r
	bne	0f
	tstb	(r4)+
	movb	(r4)+,r0
	beq	3f
	jsr	pc,findr
	mov	r1,-(sp)
	mov	r4,ibufp
	mov	$-1,eibuf
	jsr	pc,atoi
	mov	(sp)+,r1
	mov	r0,(r1)
	clr	ch
	br	3f
0:
	cmpb	(r4),$'s
	bne	0f
	inc	stop
	br	3f
0:
	cmpb	(r4),$'o
	bne	0f
	inc	old
	br	3f
0:
	cmpb	(r4),$'i
	bne	0f
	inc	stdi
	br	3f
0:
	cmpb	(r4),$'q
	bne	0f
	inc	quiet
	br	3f
0:
	cmpb	(r4),$'h
	bne	0f
	clr	slow
	br	3f
0:
	cmpb	(r4),$'n
	bne	0f
	inc	r4
	jsr	r5,pnum; npn
	br	3f
0:
	jsr	r5,pnum; pto
3:
	dec	argc
	bgt	1b
2:
4:
/	tst	index
/	beq	1f
/	sys	creat; indf; 666
/	mov	r0,indid
1:
/	mov	$nop,get1a
	clr	init
	mov	$ibuf,ibufp
	mov	$ibuf,eibuf
	mov	sp,argp
	clr	r0
1:
	movb	r0,trtab(r0)
	inc	r0
	cmp	r0,$128.
	bne	1b
	movb	$040,trtab+014
	mov	sp,ssp
	mov	$70.,vlist+6
	jsr	pc,ctime
	mov	$-1,nl
	mov	$end,enda
	clr	ioff
	jmp	loop
makebf:
	tst	(r0)+
	mov	(r0),r0
	cmpb	(r0),$'a
	bne	0f
	mov	$bfn1,r0
	mov	r0,7f
	mov	r0,8f
	mov	r0,9f
/	mov	r0,place1
0:
	sys	stat; 8:bfn; stbuf
	bec	2f
	sys	creat; 9:bfn; 600
	bec	1f
2:
	incb	bfn1+3
	cmpb	bfn1+3,$'z
	blos	0b
	jmp	place
1:
	mov	r0,ibf
	mov	$blockend,r0
	sub	$block,r0
	cmp	r0,$1024.
	blos	1f
	4
1:
	mov	r0,blocksize
	clr	-(sp)
2:
	mov	ibf,r0
	sys	write; block; 1024.
	inc	(sp)
	cmp	(sp),nev
	ble	2b

	mov	(sp)+,r3
	mpy	$1024.,r3
	mov	r3,nextb
	mov	r3,first
	mov	ibf,r0
	sys	close
	sys	open; 7:bfn; 2
	jes	place
	mov	r0,ibf
	mov	r0,ibf1
	cmp	$bfn1,7b
	beq	1f
	sys	unlink; bfn
1:
	rts	pc

ctime:
	sys	time
	sub	$18000.,r1 /5hrs for est
	sbc	r0
	ashc	$-2,r0
	div	$21600.,r0
	inc	r0
	mov	r0,dy
/	mov	r1,fd
	add	$3,r0
	mov	r0,r1
	sxt	r0
	div	$7,r0
	inc	r1
	mov	r1,dw
3:
	mov	yr,r1
	sxt	r0
	div	$4,r0
	mov	$28.,ms+2
	tst	r1
	bne	0f
	mov	$29.,ms+2
0:
	clr	r0
1:
	cmp	dy,ms(r0)
	ble	2f
	sub	ms(r0),dy
	tst	(r0)+
	cmp	r0,$24.
	blt	1b
	inc	yr
	mov	yr,r1
	br	3b
2:
	asr	r0
	inc	r0
	mov	r0,mo
	rts	pc
ms: 31.;28.;31.;30.;31.;30.;31.;31.;30.;31.;30.;31.

pnum:
	clr	-(sp)
	clr	r3
1:
	movb	(r4)+,r0
	sub	$'0,r0
	cmp	r0,$9
	bhi	1f
	inc	(sp)
	mpy	$10.,r3
	add	r0,r3
	br	1b
1:
	mov	r3,r0
	tst	(sp)+
	beq	2f
	mov	r0,*(r5)+
1:
	rts	r5
2:
	tst	(r5)+
	br	1b

emes1: <Too many files.\n\0>
.even
obuf=ibuf+512.
.=ibuf+1024.
.text
loop:
	clr	nlflg
	clr	nflush
	clr	nb
	mov	ilistp,r1
	jsr	pc,getch1
	mov	r0,ch
	cmp	ilistp,r1
	beq	1f
	tst	ejf
	beq	1f
	cmp	ilistp,ejl
	bhi	1f
	mov	ilistp,ejl
	inc	nflush
	jsr	pc,newline
	br	loop
1:
	jsr	pc,getchar
	tst	pendt
	bne	0f
	tst	lit
	ble	1f
	cmp	frame,litlev
	bhi	1f
	dec	lit
	br	0f
1:
	cmpb	r0,cc
	beq	3f
	cmpb	r0,c2
	beq	2f
	cmpb	r0,tch
	beq	4f
	cmpb	r0,$002
	beq	6f
0:
	movb	r0,ch
	jsr	pc,text
	br	loop
4:
	inc	tflg
	inc	copyf
0:
	jsr	pc,getchar
	mov	r0,r5
	jsr	pc,putchar
	cmp	r5,$'\n
	bne	0b
	clr	tflg
	clr	copyf
	br	loop
2:
	inc	nb
3:
	jsr	pc,control
5:
	jsr	pc,flushi
	br	loop
6:
	inc	raw
	jsr	pc,get1
	movb	r0,xpc
	clr	nlflg
0:
	jsr	pc,get1
	cmpb	r0,xpc
	beq	7f
	jsr	pc,pchar2
	br	0b
7:
	clr	raw
	jbr	loop

mesg:
	tst	r0
	bne	setsame
	sys	stat; ttyx; stbuf
	mov	stbuf+4,0f
	mov	0f,1f
	bic	$22,0f
	sys	0;7f
.data
7:	sys	chmod; ttyx; 0:..
.text
	rts	pc
setsame:
	sys	0;7f
.data
7:	sys	chmod; ttyx; 1:..
.text
	rts	pc

stringfl:
	jsr	pc,flush
string:
	mov	r0,-(sp)
	mov	r1,-(sp)
	mov	r2,-(sp)
	mov	(r5)+,r1
	mov	r1,r2
	mov	r1,9f
1:
	tstb	(r1)+
	bne	1b
	dec	r1
	sub	r2,r1
	mov	r1,1f
	mov	ttyod,r0
	cmp	r0,ibf
	beq	2f
0:
	sys	0;7f
.data
7:	sys	write; 9:..; 1:..
.text
	bes	2f
1:
	mov	(sp)+,r2
	mov	(sp)+,r1
	mov	(sp)+,r0
	rts	r5
2:
	sys	creat; tmfile; 666
	bes	1b
	mov	r0,ttyod
	br	0b

flushi:
	tst	nflush
	bne	1f
	clr	ch
	tst	nlflg
	bne	1f
	tst	donef
	beq	2f
	cmp	$ilist,ilistp
	beq	1f
2:
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
	cmp	r0,$016
	beq	4f
	cmp	r0,$017
	beq	4f
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
	ble	4f
3:
	mov	$1,ulstate
	mov	r0,ch
	br	gettchar
2:
	inc	bsc
	inc	ulc
4:
	tst	r0
	rts	pc

rtnch:
	mov	ch,r0
	clr	ch
	tst	r0
	rts	pc

getchar:
	mov	r1,-(sp)
1:
	jsr	pc,getch1
	bmi	2f
	cmpb	r0,fc
	bne	2f
	tst	copyf
	bne	2f
	jsr	pc,setfield
	br	1b
2:
	mov	(sp)+,r1
	tst	r0
	rts	pc
getch1:
	tst	ch
	bne	rtnch
	mov	r1,-(sp)
1:
	jsr	pc,getch0
	bmi	2f
	tst	copyf
	bne	2f
	cmpb	r0,$030 /\w
	bne	2f
	jsr	r5,setwd;getch0
	br	1b
2:
	jsr	pc,width
	add	r1,column
	mov	(sp)+,r1
	tst	r0
	rts	pc

getch0:
	tst	ch
	bne	rtnch
1:
	tst	nlflg
	beq	1f
	mov	$'\n,r0
	rts	pc
1:
	mov	r1,-(sp)
1:
	jsr	pc,get1
	cmpb	r0,eschar
	bne	2f
	jsr	pc,get1
	jsr	r5,switch; esctab
	cmpb	r0,$'\n
	bne	3f
/	clr	column
	br	1b
2:
	cmp	r0,$033  /prefix
	bne	3f
	jsr	pc,get1
	jsr	r5,switch; pfxtab
3:
	cmpb	r0,$376 /comment
	bne	2f
0:
	jsr	pc,get1
	bmi	0b
	cmpb	r0,$012
	bne	0b
2:
	cmpb	numc,r0
	bne	2f
	clr	r1
	jsr	pc,setn
	br	1b
2:
	cmpb	r0,$372	/mark hor place
	bne	2f
	tst	copyf
	bne	2f
	mov	r0,r1
	jsr	pc,setn
	jsr	pc,findr
	jeq	1b
	mov	column,(r1)
	jbr	1b
2:
	cmpb	r0,$025	/text string
	bne	2f
	jsr	pc,setstr
	br	1b
2:
	cmpb	dolc,r0
	bne	2f
	jsr	pc,seta
	br	1b
2:
	cmpb	r0,$026 /font indicator
	bne	2f
	jsr	pc,get1
	br	1b
2:
	cmpb	r0,$027 /point size
	bne	2f
	jsr	pc,eatps
	br	1b
2:
	cmpb	r0,$021 /spead line
	bne	2f
	tst	copyf
	bne	2f
	inc	spread
	br	1b
2:
	cmpb	r0,$006 /repeat
	bne	2f
	jsr	pc,setrpt
	br	1b
2:
	cmp	r0,$'\n
	bne	3f
	inc	nlflg
	clr	column
	tst	ip
	bne	3f
	inc	iline
3:
	mov	(sp)+,r1
	tst	r0
	rts	pc

eatps:
	jsr	pc,get1
	cmpb	r0,$'+
	beq	0f
	cmpb	r0,$'-
	beq	0f
	sub	$'0,r0
	ble	1f
	cmp	r0,$3
	bgt	1f
0:
	jsr	pc,get1
1:
	rts	pc

.data
esctab:
.byte '*, 025  /text string
.byte 'n, 034  /number char
.byte '$, 020  /dollar char
eschar:
.byte '\\, 134 /backslash
.byte 'e,  013 /printable escape char
.byte 'f, 026  /font indicator
.byte 's, 027  /point size
.byte '&, 037 /filler
.byte 't, 011  /hor tab
.byte '!, 024  /transparent char
.byte 'p, 021  /spread line
.byte 'c, 005 /interrupted text
.byte 'k, 372 /mk hor
/ 014 hidden tab replacement character
.byte ' , 014 /\(space)
.byte 'x, 016  /SO (extra chars)
.byte 'y, 017  /SI (normal characters)
.byte 'l, 0177 /delete
.byte 'd, 032  /hlf (down)
.byte 'u, 035  /hlr (up)
.byte 'r, 036  /flr (reverse)
.byte 'a, 001  /leader char
.byte ':, 003 /lem char
.byte '?, 002 /raw trans
.byte '", 376 /comment
.byte 'w, 030  /width size char
.byte 0, 0
.text

pfxtab:
   .byte '7, 036  /flr
   .byte '8, 035  /hlr
   .byte '9, 032  /hlf
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
	mov	rchar,r0
	rts	pc
1:
	mov	r1,-(sp)
4:
	tst	cp
	beq	2f
	movb	*cp,r0
	bne	1f
	clr	cp
	br	4b
1:
	inc	cp
	br	8f
2:
	tst	ap
	beq	2f
	movb	*ap,r0
	bne	1f
	clr	ap
	br	4b
1:
	inc	ap
	br	8f
2:
	cmp	$-1,ip
	bne	1f
	jsr	pc,rdtty
	br	8f
1:
	tst	ip
	beq	5f
	jsr	pc,rbf
	br	8f
5:
	tst	donef
	beq	0f
	jmp	done
0:
	tst	nx
	bne	0f
3:
	mov	ibufp,r1
	cmp	r1,eibuf
	bne	3f
	cmp	r1,$-1
	beq	3f
0:
	tst	nfo
	bne	2f
1:
	jsr	pc,nextfile
	bne	3b
2:
	clr	nx
	mov	ifile,r0
	sys	read; ibuf; 512.
	bes	1b
	tst	r0
	beq	1b
	mov	$ibuf,r1
	add	r1,r0
	mov	r0,eibuf
3:
	movb	(r1)+,r0
	bic	$!377,r0
	inc	ioff
	mov	r1,ibufp
	tst	raw
	bne	6f
	cmpb	r0,$040
	bhis	8f
	mov	r0,-(sp)
	mov	cbits,r0
	mov	cbits+2,r1
	ashc	(sp),r0
	bmi	0f
	clr	(sp)
0:
	mov	(sp)+,r0
8:
	tst	raw1
	bne	6f
	cmp	r0,$004
	beq	4b
	tst	copyf
	bne	6f
	cmpb	r0,$011	/tab
	bne	0f
	mov	tabc,rchar
	br	1f
0:
	cmpb	r0,$001
	bne	6f
	mov	dotc,rchar
1:
	cmpb	r0,fc
	beq	6f
	cmpb	r0,padc
	beq	6f
	mov	(sp)+,r1
	mov	$tabtab,r0
	inc	nspace
1:
	tst	(r0)
	jeq	get1
	cmp	column,(r0)+
	bge	1b
	mov	-(r0),nspace
	sub	column,nspace
	jbr	get1
6:
/get1a:	br	7f
	tst	init
	bne	7f
	tst	r0
	jeq	4b
7:
	mov	(sp)+,r1
	tst	r0
	rts	pc
cbits: 040743;20 /001,007-012,033 (soh,bel,bs,tab,nl,so,si,esc)
/cbits: 041743;20 /001,006-012,033 (soh,ack,bel,bs,tab,nl,so,si,esc)

edone:
	mov	$stk,frame
	clr	ip
done:
	clr	app
	clr	ds
	mov	em,r0
	beq	0f
	clr	em
	mov	pc,donef
	mov	frame,-(sp)
	jsr	pc,cont1
	cmp	(sp)+,frame
	bne	1f
0:
	tst	nfo
	beq	3f
	clr	op
	clr	mflg
	tst	woff
	beq	0f
	clr	r0
	jsr	pc,wbt
0:
	clr	pendnf
	tst	pendw
	beq	0f
	clr	*pendw
	clr	pendw
0:
	cmp	$1,donef
	beq	done1
	mov	$1,donef
	clr	ip
	mov	$ilist,ilistp
	jsr	pc,rbreak
	inc	nflush
	mov	$ilist,r0
	jsr	pc,eject
1:
	mov	ssp,sp
	jmp	loop
done1:
/	tst	pendb
/	bne	0b
	tst	nl
	beq	1f
	jsr	pc,newline
	br	1b
1:
3:
done2:
	jsr	pc,flush
place:
	sys	signal; 2; 1
	tst	quiet
	beq	1f
	bis	$10,ttym
	mov	ttyid,r0
	sys	stty; ttys
1:
	mov	$1,r0
	jsr	pc,mesg
	mov	outid,r0
	sys	close
	sys	wait
	sys	exit

nextfile:
	mov	ifile,r0
	beq	1f
	sys	close
1:
	tst	nx
	beq	2f
	mov	$nextf,r0
	br	3f
2:
	cmp	ifp,$ifl
	beq	1f
	jsr	pc,popf
	bne	nextfile
	tst	pc
	rts	pc
1:
	dec	argc
	blt	4f
	mov	*argp,r0
	add	$2,argp
3:
	mov	r0,5f
	cmpb	(r0)+,$'-
	bne	0f
	tstb	(r0)
	bne	0f
	clr	r0
	br	1f
0:
	sys	0;7f
.data
7:	sys	open; 5:..; 0
.text
	bec	1f
	sub	mflg,nfo
	jgt	done
	jsr	r5,string; omes
	mov	5b,8f
	jmp	7f
.data
7:	jsr	r5,string; 8:..
	jmp	6f
.text
6:
	jsr	r5,string; 9f
	jbr	done
1:
	clr	iline
	inc	nfo
	mov	r0,ifile
	clr	ioff
	rts	pc
4:
	sub	mflg,nfo
	beq	0f
	tst	stdi
	jeq	done
0:
	clr	iline
	clr	mflg
	inc	nfo
	clr	stdi
	clr	ifile
	clr	ioff
	rts	pc
omes: <Cannot open: \0>
9: <\n\0>
.even

popf:
	clr	-(sp)
	mov	ifp,r1
	cmp	r1,$ifl
	beq	1f
	sub	$2,ifp
	mov	-(r1),ifile
	mov	[offl-ifl](r1),ioff
	mov	ioff,0f
	bic	$777,0f
	mov	ifile,r0
	beq	4f
	sys	0;7f
.data
7:	sys	seek; 0:..; 0
.text
	bes	2f
	mov	ifile,r0
	sys	read; ibuf; 512.
	bes	2f
	add	$ibuf,r0
	mov	r0,eibuf
	sub	ioff,0b
	mov	$ibuf,ibufp
	mov	ifile,r0
	jsr	pc,ttyn
	cmpb	r0,$'x
	bne	1f
	sub	0b,ibufp
	cmp	ibufp,eibuf
	blo	1f
2:
	inc	(sp)
1:
	tst	(sp)+
	rts	pc
4:
	jsr	pc,restbuf
	br	1b

savebuf:
	mov	$ibuf,r0
	mov	$xbuf,r1
	mov	ibufp,xbufp
	mov	eibuf,xeibuf
1:
	mov	(r0)+,(r1)+
	cmp	r0,eibuf
	blo	1b
	rts	pc

restbuf:
	mov	$xbuf,r0
	mov	$ibuf,r1
	mov	xbufp,ibufp
	mov	xeibuf,eibuf
1:
	mov	(r0)+,(r1)+
	cmp	r1,eibuf
	blo	1b
	rts	pc

putchar:
	bic	$!377,r0
	beq	2f
	tstb	r0
	bmi	pchar2
	cmpb	r0,$013 /\e
	bne	0f
	tst	op
	bne	0f
	movb	eschar,r0
	br	putchar
0:
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
	br	pchar1
1:
	mov	$011,-(sp)
1:
	tst	nsp
	beq	2f
	tst	slow
	bne	4f
	tst	op
	bne	4f
	jsr	pc,dsp
	cmp	nsp,r1
	blt	4f
	mov	$011,(sp)
	cmp	r1,$1
	bgt	8f
	mov	$040,(sp)
	dec	nsp
	br	9f
8:
	sub	r1,nsp
9:
	mov	r0,-(sp)
3:
	mov	2(sp),r0
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
	tst	(sp)+
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
pchar2:
	tst	op
	beq	1f
	mov	op,r1
	jsr	pc,wbf
	mov	r1,op
	br	3f
1:
	tst	tflg
	bne	1f
	cmp	pn,pfrom
	blt	3f
	clr	pfrom
1:
	movb	r0,*obufp
	inc	obufp
	cmp	obufp,$obuf+512.
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
	mov	outid,r0
	sys	0;7f
.data
7:	sys	write; obuf; 0:0
.text
	jes	place
	inc	toolate
	mov	$obuf,obufp
	rts	pc

rdtty:
/	mov	sp,r1
/	sys	signal; 2; rdtty1
	clr	r0
	sys	read; char; 1
	tst	r0
	bne	2f
rdtty1:
/	mov	r1,sp
	jsr	pc,popi
rdtty2:
	clr	tty
	tst	quiet
	beq	0f
	bis	$10,ttym
	mov	ttyid,r0
	sys	stty; ttys
0:
	clr	r0
1:
/	sys	signal; 2; place
	rts pc
2:
	mov	char,r0
	cmpb	r0,$'\n
	beq	3f
	mov	$1,tty
	br	1b
3:
	inc	tty
	cmp	tty,$3
	beq	rdtty1
	br	1b

ttyn:
	mov	r1,-(sp)
	mov	$'x,-(sp)
	clr	-(sp)
	sys	fstat; ybuf
	bes	3f
	mov	ybuf+2,(sp)
	sys	open; dev; 0
	bes	3f
	mov	r0,r1
1:
	mov	r1,r0
	sys	read; ybuf; 16.
	bes	2f
	cmp	r0,$16.
	bne	2f
	mov	$ybuf,r0
	cmp	(r0)+,(sp)
	bne	1b
	cmp	(r0)+,$"tt
	bne	1b
	cmpb	(r0)+,$'y
	bne	1b
	tstb	(r0)+
	beq	1b
	cmpb	(r0),$'\0
	bne	1b
	movb	-(r0),2(sp)
2:
	mov	r1,r0
	sys	close
3:
	tst	(sp)+
	movb	(sp)+,r0
	mov	(sp)+,r1
	rts	pc
dev:	</dev\0>
.even

cont1:
	mov	r0,-(sp)
	mov	pc,r2
	br	0f
control:
	jsr	pc,getchar
	mov	r0,-(sp)
	jsr	pc,getchar
	cmpb	r0,$'\n
	beq	8f
	cmpb	r0,$' /
	bne	9f
8:
	mov	r0,ch
	clr	r0
9:
	swab	r0
	bis	(sp),r0
	clr	r2
0:
	mov	$contab,r1
1:
	mov	(r1)+,(sp)
	bic	$100000,(sp)
	cmp	r0,(sp)
	bne	4f
	mov	(r1),(sp)
	tst	-(r1)
	bpl	3f
	clr	*nxf
	tst	r2
	bne	2f
	jsr	pc,collect
2:
	jsr	pc,flushi
	jsr	pc,pushi
	beq	5f
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

.data
contab:
<ad>; casead
<bp>; casebp
<pn>; casepn
<br>; casebr
<cc>; casecc
<c2>; casec2
<ce>; casece
<fi>; casefi
<in>; casein
<li>; caseli
<ll>; casell
<ls>; casels
<ns>; casens
<rs>; casers
<na>; casena
<ne>; casene
<nf>; casenf
<pl>; casepl
<sp>; casesp
<lv>; casesv
<sv>; casesv
<os>; caseos
<ta>; caseta
<ti>; caseti
<tr>; casetr
<ul>; caseul
<tl>; casetl
<lt>; caselt
<hc>; casehc
<hy>; casehy
<nh>; casenh
<nm>; casenm
<np>; casenp
<nn>; casenn
<ar>; casear
<ro>; casero
<RO>; caseroc
<nx>; casenx
<so>; caseso
<po>; casepo
<de>; casede
<ds>; caseds
<am>; caseam
<as>; caseas
<da>; caseda
<di>; casedi
<rm>; caserm
<rn>; casern
<ig>; caseig
<tc>; casetc
<ec>; caseec
<eo>; caseeo
<lc>; caselc
<nc>; casenc
<ev>; caseev
<if>; caseif
<wh>; casewh
<ch>; casech
<rd>; caserd
<tm>; casetm
<nr>; casenr
<mk>; casemk
<rt>; casert
<ab>; casest
<fl>; casefl
<ex>; done
<xh>; casexh
<em>; caseem
<fc>; casefc
<af>; caseaf
<pi>; casepi
<hw>; casehw
bnames: .=.+512.
-1; -1
.text
