/
/

/ nroff4

text:
	inc	nflush
	cmp	nl,$-1
	bne	0f
	jsr	pc,newln1
	rts	pc
0:
	clr	ulstate
	jsr	pc,setnel
	tst	ce
	bne	nofill
	tst	fi
	beq	nofill
	tst	pendw
	bne 7f
	tst	pendt
	bne	8f
	inc	pendt
	tst	x
	bne	0f
1:
	jsr	pc,getchar
	bmi	1f
	cmpb	r0,$' /
	beq	3f
	cmpb	r0,tabc
	bne	1f
3:
	inc	x
	br	1b
1:
	tst	nlflg
	bne	6f
	mov	r0,ch
	tst	x
	beq	2f
0:
	jsr	pc,rbreak
	tst	nc
	bne	5f
	tst	wch
	bne	5f
	add	x,un
	clr	x
	jsr	pc,setnel
	tst	trap
	bne	5f
2:
	tst	x
	bne	0b
	tst	nlflg
	beq	2f
6:
	clr	nflush
	clr	x
	clr	pendt
	clr	ch
	mov	$1,r0
	jsr	pc,casesp1
	rts	pc
8:
	tst	x
	bne	0b
2:
	tst	spread
	bne	1f
	tst	pendw
	bne	0f
	tst	wch
	bne	3f
0:
7:
	jsr	pc,getword
		br 4f
3:
	jsr	pc,movword
	bne	2b
	tst	nlflg
	beq	1f
	clr	pendt
1:
	jsr	pc,adjust
	tst	trap
	beq	2b
	tst	nlflg
	beq	5f
4:
	clr	pendt
	tst	pendw
	bne	5f
	dec	ul
	bge	5f
	clr	ul
5:
	clr	nflush
	rts	pc

nofill:
	tst	pendnf
	bne	1f
	clr	over
	jsr	pc,rbreak
	tst	trap
	bne	3f
	tst	nlflg
	bne	6b
	clr	fac
	clr	fmq
	mov	$1000,nwd
1:
	jsr	pc,gettchar
	bmi	0f
	cmpb	r0,$'\n
	beq	1f
	cmpb	r0,ohc
	beq	1b
	cmpb	r0,$005
	beq	4f
0:
	jsr	pc,storeline
	br	1b
1:
	tst	ce
	ble	2f
	dec	ce
	mov	nel,r0
	asr	r0
	bpl	1f
	clr	r0
1:
	add	r0,un
2:
	tst	nc
	bne	0f
	mov	$037,r0
	jsr	pc,storeline
0:
	jsr	pc,rbreak
	dec	ul
	bpl	3f
	clr	ul
3:
	clr	nflush
	clr	pendnf
	rts	pc
4:
	inc	pendnf
	clr	nflush
	jsr	pc,flushi
	rts	pc

adjust:
	mov	r2,-(sp)
	mov	r3,-(sp)
	clr	r2
	clr	r3
	tst	ad
	beq	1f
	mov	nwd,r0
	dec	r0
	ble	1f
	mov	nel,r3
	ble	1f
	dvd	r0,r2
1:
	mov	r3,fac
	mov	r2,fmq
	mov	(sp)+,r3
	mov	(sp)+,r2
	mov	$-1,lastl
	jsr	pc,rbreak1
	clr	spread
	rts	pc

fill:
	mov	fmq,r0
1:
	inc	r0
	dec	nc
	cmpb	(r2)+,$' 
	beq	1b
	dec	r2
	bit	$1,totout
	beq	2f
	inc	fac
	cmp	fac,nwd
	blt	1f
	inc	r0
	br	1f
2:
	dec	fac
	bmi	1f
	inc	r0
1:
	jsr	pc,space
	movb	(r2),r0
	rts	pc

movword:
	mov	wordp,r4
	tst	nwd
	bne	2f
1:
	movb	(r4)+,r0
	cmp	r0,$' 
	bne	1f
	dec	wch
	jsr	pc,width
	sub	r1,wne
	br	1b
1:
	dec	r4
2:
	cmp	wne,nel
	ble	1f
	tst	nwd
	beq	2f
	cmp	nel,$4
	blt	1f
	cmp	wch,$5 /don't do 4 char words
	ble	1f
2:
	bit	$2,hyf
	beq	0f
	tst	op
	bne	0f
	clr	r0
	jsr	pc,findt
	dec	r1
	ble	1f
0:
	jsr	pc,hyphen
1:
	clr	nhyph
	mov	wch,-(sp)
1:
	movb	(r4)+,r0
	cmp	r0,$'-
	bne	2f
	movb	(r4),r2
	jsr	pc,alph2
	bne	2f
	bisb	$200,(r4)
2:
	tst	r0
	bpl	2f
	bic	$!177,r0
	mov	r4,r3
	sub	$3,r3
	cmp	r3,$word
	blo	2f
	movb	(r3),r2
	bic	$!177,r2
	jsr	pc,alph2
	bne	2f
3:
	mov	r0,-(sp)
	clr	r0
	jsr	pc,storeline
	mov	(sp)+,r0
	inc	nhyph
2:
	jsr	pc,width
	sub	r1,wne
	jsr	pc,storeline
	dec	wch
	bne	1b
	tst	nel
	blt	1f
	inc	nwd
	tst	(sp)+
	clz
	rts	pc
1:
	mov	linep,r3
1:
	tst	nhyph
	bne	2f
	tst	nwd
	beq	3f
	cmp	wch,(sp)
	beq	4f
2:
	movb	-(r3),r0
	bne	2f
	dec	nhyph
	bne	5f
	tst	nwd
	beq	6f
5:
	tst	nel
	ble	2f
6:
	cmpb	-1(r3),$'-
	beq	3f
	movb	$'-,(r3)
	dec	nel
	inc	ne
	br	3f
2:
	dec	nc
	tstb	(r3)
	beq	1b
	jsr	pc,width
	sub	r1,ne
	add	r1,nel
	inc	wch
	dec	r4
	add	r1,wne
	br	1b
3:
	inc	nwd
4:
	mov	r4,wordp
	bicb	$!177,(r4)
	tst	(sp)+
	sez
	rts	pc


width:
	mov	r0,-(sp)
	cmpb	r0,$014
	bne	0f
	mov	$1,r1
	br	3f
0:
	cmpb	r0,$013
	bne	0f
	movb	eschar,r0
0:
	bic	$!177,r0
	cmp	r0,ohc
	beq	2f
	tst	r0
	beq	2f
	cmp	r0,$0177
	beq	2f
	cmp	r0,$010
	bne	1f
	mov	$-1,r1
	br	3f
1:
	cmp	$' ,r0
	bgt	2f
	mov	$1,r1
	br	3f
2:
	clr	r1
3:
	mov	(sp)+,r0
	rts	pc
setwd:
	mov	column,-(sp)
	clr	column
	mov	r2,-(sp)
	clr	-(sp)
	jsr	pc,*(r5)
	mov	r0,r2
1:
	jsr	pc,*(r5)
	bmi	0f
	cmpb	r0,r2
	beq	2f
0:
	tst	nlflg
	bne	2f
	jsr	pc,width
	add	r1,(sp)
	br	1b
2:
	mov	(sp)+,r0
	jsr	pc,setn0
	mov	(sp)+,r2
	mov	(sp)+,column
	tst	(r5)+
	rts	r5

header:
/headin:
	clr	nls
	jsr	pc,skipcont
	mov	$'',r2
	tst	op
	beq	1f
	jsr	pc,wbfl
1:
	jsr	pc,alloc
	bne	0f
	rts	pc
0:
	mov	nextb,r1
2:
	mov	r1,headp
	jsr	pc,getchar
	bmi	3f
	cmpb	r0,$'\n
	beq	2f
	mov	r0,r2
1:
	jsr	pc,getchar
	bmi	3f
	cmpb	r0,$'\n
	beq	2f
0:
	cmpb	r0,r2
	bne	3f
	mov	$004,r0
3:
	jsr	pc,wbf
	br	1b
2:
	mov	$004,r0
	jsr	pc,wbf
	mov	$004,r0
	jsr	pc,wbf
	clr	r0
	jsr	pc,wbt

/headout:
	clr	-(sp)
	mov	headp,r2
	jsr	r5,headseg; width
	mov	r0,-(sp)
	jsr	r5,headseg; width
	mov	r0,-(sp)
	jsr	r5,headseg; width
	mov	r0,-(sp)
	tst	op
	bne	1f
	mov	po,r0
	jsr	pc,space
1:
	mov	headp,r2
	jsr	r5,headseg; putchar
	mov	llh,r0
	add	6(sp),r0
	sub	2(sp),r0
	asr	r0
	sub	4(sp),r0
	mov	r0,-(sp)
	tst	4(sp)
	bne	1f
	tst	2(sp)
	beq	2f
1:
	jsr	pc,space
2:
	jsr	r5,headseg; putchar
	mov	llh,r0
	sub	(sp)+,r0
	sub	(sp)+,r0
	sub	(sp)+,r0
	sub	(sp)+,r0
	add	(sp)+,r0
	tst	-10(sp)
	beq	1f
	jsr	pc,space
	jsr	r5,headseg; putchar
1:
	jsr	pc,newline
	cmp	nl,hnl
	ble	0f
	mov	nl,hnl
0:
	mov	headp,r0
	jsr	pc,free
	rts	pc

headseg:
	clr	-(sp)
1:
	mov	r1,-(sp)
	mov	r2,r1
	jsr	pc,rbf0
	jsr	pc,incoff
	mov	r1,r2
	mov	(sp)+,r1
	tst	r0
	beq	1f
	bmi	0f
	cmpb	r0,$004
	beq	1f
	cmpb	r0,$'%
	beq	2f
0:
	jsr	pc,*(r5)
	add	r1,(sp)
	br	1b
2:
	mov	$'%,r0
	jsr	pc,findr
	mov	[flist-vlist](r1),nform
	mov	pn,r0
	clr	r1
	jsr	pc,fnumb
	add	r1,(sp)
	br	1b
1:
	mov	(sp)+,r0
	tst	(r5)+
	rts	r5

space:
	jsr	r5,nlines;putchar
	rts	pc

nlines:
	mov	r0,-(sp)
1:
	dec	(sp)
	blt	1f
	mov	$' ,r0
	jsr	pc,*(r5)
	br	1b
1:
	cmp	(r5)+,(sp)+
	rts	r5

decimal:
	jsr	pc,decml
	tst	(r5)+
	rts	r5

decml:
	mov	r2,-(sp)
	mov	r3,-(sp)
	jsr	pc,decml1
	mov	(sp)+,r3
	mov	(sp)+,r2
	rts	pc

decml1:
	mov	r1,-(sp)
	clr	r2
	mov	r0,r3
	dvd	$10.,r2
	mov	r3,-(sp)
	mov	r2,r0
	beq	1f
	jsr	pc,decml1
	mov	r1,2(sp)
1:
	mov	(sp)+,r0
	add	$'0,r0
	jsr	pc,*(r5)
	add	(sp)+,r1
	rts	pc

roman0:
	jsr	pc,roman
	tst	(r5)+
	rts	r5
roman:
	tst	r0
	beq	decml
	mov	r2,-(sp)
	mov	r3,-(sp)
	cmp	ro,$2
	bne	1f
	mov	$cones,onesp
	mov	$cfives,fivesp
	br	2f
1:
	mov	$ones,onesp
	mov	$fives,fivesp
2:
	jsr	pc,roman1
	mov	(sp)+,r3
	mov	(sp)+,r2
	rts	pc
roman1:
	clr	r2
	mov	r0,r3
	bne	.+4
	rts	pc
	mov	r1,-(sp)
	dvd	$10.,r2
	mov	r3,-(sp)
	mov	r2,r0
	inc	onesp
	inc	fivesp
	jsr	pc,roman1
	mov	r1,2(sp)
	dec	onesp
	dec	fivesp
	clr	r2
	mov	(sp)+,r3
	dvd	$5.,r2
	cmp	r3,$4
	bne	1f
	movb	*onesp,r0
	jsr	pc,*(r5)
	add	r1,(sp)
	tst	r2
	beq	2f
	inc	onesp
	movb	*onesp,r0
	dec	onesp
	br	3f
2:
	movb	*fivesp,r0
3:
	jsr	pc,*(r5)
	add	(sp)+,r1
	rts	pc
1:
	tst	r2
	beq	2f
	movb	*fivesp,r0
	jsr	pc,*(r5)
	add	r1,(sp)
2:
	dec	r3
	blt	1f
	movb	*onesp,r0
	jsr	pc,*(r5)
	add	r1,(sp)
	br	2b
1:
	mov	(sp)+,r1
	rts	pc

abc0:
	jsr	pc,abc
	tst	(r5)+
	rts	r5
abc:
	mov	r2,-(sp)
	mov	r3,-(sp)
	mov	r0,r3
	bne	0f
	mov	$'0,r0
	jsr	pc,*(r5)
	br	1f
0:
	dec	r3
	jsr	pc,abc1
1:
	mov	(sp)+,r3
	mov	(sp)+,r2
	rts	pc
abc1:
	clr	r2
	dvd	$26.,r2
	mov	r3,-(sp)
	mov	r2,r3
	beq	1f
	dec	r3
	jsr	pc,abc1
1:
	cmp	ro,$2
	beq	1f
	add	$'a,(sp)
	br	2f
1:
	add	$'A,(sp)
2:
	mov	(sp)+,r0
	jsr	pc,*(r5)
	rts	pc

fnumb0:
	jsr	pc,fnumb
	tst	(r5)+
	rts	r5
fnumb:
	mov	ro,-(sp)
	mov	nform,ro
	bne	0f
	mov	(sp),ro
	br	1f
0:
	dec	ro
1:
	tst	ro
	bne	1f
	jsr	pc,decml
	br	2f
1:
	cmp	ro,$2
	bgt	1f
	jsr	pc,roman
	br	2f
1:
	sub	$2,ro
	jsr	pc,abc
2:
	mov	(sp)+,ro
	rts	pc

/wordout:
/	tst	index
/	beq	1f
/	mov	wch,0f
/	beq	1f
/	mov	indid,r0
/	sys	write; word; 0:..
/	mov	indid,r0
/	sys	write; 8f; 1
/	mov	indid,r0
/	jsr	r5,numb; pn
/	mov	indid,r0
/	sys	write; 8f; 1
/	mov	nl,char
/	inc	char
/	mov	indid,r0
/	jsr	r5,numb; char
/	mov	indid,r0
/	sys	write; 9f; 1
/1:
/	rts	pc
/8:	011 /tab
/9:	<\n\0>
/	.even
/numb:
/	mov	r2,-(sp)
/	mov	r3,-(sp)
/	mov	*(r5)+,r3
/	mov	r0,r1
/	jsr	pc,numb1
/	mov	(sp)+,r3
/	mov	(sp)+,r2
/	rts	r5
/numb1:
/	clr	r2
/	dvd	$10.,r2
/	mov	r3,-(sp)
/	mov	r2,r3
/	beq	1f
/	jsr	pc,numb1
/1:
/	add	$'0,(sp)
/	mov	(sp)+,char
/	mov	r1,r0
/	sys	write; char; 1
/	rts	pc

setstr:
	clr	-(sp)
	clr	-(sp)
0:
	jsr	pc,get1
	bmi	3f
	bic	$!177,r0
	cmpb	r0,$'\\
	bne	1f
4:
	jsr	pc,get1
	jsr	r5,switch;esctab
	cmpb	r0,dolc
	bne	2f
	jsr	pc,seta
	br	0b
2:
	cmpb	r0,numc
	bne	1f
	clr	r1
	jsr	pc,setn
	br	0b
1:
	tst	2(sp)
	bne	5f
	bic	$!177,r0
	cmpb	r0,$'(
	bne	1f
	inc	2(sp)
	jsr	pc,get1
	bic	$!177,r0
	cmpb	r0,$'\\
	beq	4b
5:
	mov	r0,-(sp)
	jsr	pc,get1
	bic	$!177,r0
	swab	r0
	bis	(sp)+,r0
1:
	mov	$contab,r1
1:
	mov	(r1)+,(sp)
	bic	$100000,(sp)
	cmp	r0,(sp)
	beq	2f
	cmp	(r1)+,$-1
	bne	1b
	br	3f
2:
	mov	(r1),(sp)
	tst	-(r1)
	bpl	3f
	clr	*nxf
	jsr	pc,pushi
	mov	(sp),ip
3:
	cmp	(sp)+,(sp)+
	rts	pc

copys:
	inc	copyf
	jsr	pc,skipcont
	bne	2f
	jsr	pc,getchar
	bmi	0f
	cmpb	r0,$'"
	bne	0f
1:
	jsr	pc,getchar
	tst	nlflg
	bne	2f
0:
	jsr	pc,wbf
	br	1b
2:
	clr	r0
	jsr	pc,wbt
	clr	copyf
	rts	pc

setrpt:
	inc	copyf
	inc	raw1
	jsr	pc,get1
	dec	copyf
	dec	raw1
	mov	r0,r1
	jsr	pc,get1
	cmpb	r0,$006
	beq	1f
0:
	movb	r0,rchar
	movb	r1,nspace
	cmpb	r1,$377
	bne	1f
	clr	nspace
1:
	rts	pc

setfield:
	clr	npad
	mov	r1,-(sp)
	mov	r2,-(sp)
	mov	r3,-(sp)
	mov	r4,-(sp)
	clr	r4
	clr	-(sp)
	jsr	pc,width
	sub	r1,column /start
	mov	column,-(sp)
	mov	$tabtab,r0
1:
	tst	(r0)
	beq	4f
	cmp	(sp),(r0)+
	bge	1b
	mov	-(r0),2(sp)
	sub	(sp),2(sp) /h
	mov	$fbuf,r2
1:
	jsr	pc,getch1
	bmi	0f
	cmpb	r0,padc
	bne	2f
	jsr	pc,width
	sub	r1,column
	inc	npad
	mov	r2,-(sp)
	add	$2,r2
2:
	cmpb	r0,fc
	beq	3f
	cmpb	r0,$'\n
	bne	0f
	mov	$012,r4
	clr	nlflg
	br	3f
0:
	movb	r0,(r2)+
	cmp	r2,$efbuf-6
	blo	1b
3:
	tst	npad
	bne	0f
	inc	npad
	mov	r2,-(sp)
	add	$3,r2
0:
	movb	r4,(r2)+
	clrb	(r2)+
	jsr	pc,width
	sub	r1,column
	mov	npad,r1
	asl	r1
	add	sp,r1
	mov	(r1),-(sp)
	sub	column,(r1) /-s
	mov	(sp)+,column
	add	2(r1),(r1) /h-s
	mov	(r1),r1
	mov	r1,r2
	sxt	r0
	div	npad,r0
	mov	$014,r3 /unpaddable sp
	tst	r2
	bpl	1f
	neg	r0
	neg	r1
	mov	$010,r3
1:
	mov	(sp)+,r2
	movb	$006,(r2)+
	movb	r0,(r2)+
	movb	r3,(r2)
	tst	r1
	beq	0f
	incb	-1(r2)
	dec	r1
0:
	tstb	-(r2)
	bne	0f
	movb	$377,(r2)
0:
	dec	npad
	bgt	1b
	mov	$fbuf,cp
4:
	cmp	(sp)+,(sp)+
	mov	(sp)+,r4
	mov	(sp)+,r3
	mov	(sp)+,r2
	mov	(sp)+,r1
	rts	pc
