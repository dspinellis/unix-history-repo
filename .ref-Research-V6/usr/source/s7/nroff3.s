/
/

/ nroff3

skipcont:
/	mov	r2,-(sp)
0:
	jsr	pc,getchar
/	mov	r0,r2
/	jsr	pc,alph2
/	beq	0b
1:
	cmp	$' ,r0
	bne	1f
	jsr	pc,getchar
	br	1b
1:
/	mov	(sp)+,r2
	mov	r0,ch
	tst	nlflg
	rts	pc

rbreak:
	jsr	pc,rbreak1
	clr	totout
	rts	pc
rbreak1:
	clr	trap
	tst	nb
	beq	1f
	rts	pc
1:
	cmp	nl,$-1
	bne	1f
	jsr	pc,newln1
	rts	pc
1:
	tst	nc
	bne	1f
	tst	pendw
	bne	4f
	tst	wch
	beq	4f
	jsr	pc,setnel
	jsr	pc,movword
	nop
1:
	clrb	*linep
	clr	nls
	inc	totout
	tst	lastl
	bmi	1f
	mov	ne,lastl
	br	0f
1:
	mov	ll,lastl
0:
	tst	op
	bne	0f
	mov	po,r0
	jsr	pc,space
0:
	jsr	pc,donum
	mov	un,r0
	jsr	pc,space
	jsr	pc,jfo
	mov	$line,r2
1:
	movb	(r2)+,r0
	cmp	$' ,r0
	bne	2f
	jsr	pc,fill
	tst	nc
	bne	1b
	br	3f
2:
	jsr	pc,putchar
	dec	nc
	bgt	1b
3:
	clr	nwd
	clr	ne
	mov	in,un
	jsr	pc,setnel
	jsr	pc,newline
	cmp	nl,hnl
	ble	0f
	mov	nl,hnl
0:
	tst	trap
	bne	3f
	mov	ls,r2
	dec	r2
	clr	r0
	jsr	pc,findt
	cmp	r1,r2
	bgt	1f
	mov	r1,r0
	jsr	r5,nlines; newline
	br	3f
1:
	mov	r2,r0
	jsr	r5,nlines;newline
3:
/	clr	pendb
	clr	spread
	rts	pc
4:
	jsr	pc,setnel
	rts	pc

jfo:
	tst	jfomod
	beq	1f
	mov	fac,r0
	add	fmq,r0
	beq	1f
	clr	fac
	clr	fmq
	mov	nel,r0
	cmp	jfomod,$1
	bne	2f
	asr	r0
2:
	jsr	pc,space
1:
	rts	pc

donum:
	tst	numbmod
	beq	2f
	dec	nn
	blt	1f
0:
	mov	$3,r0
	add	nms,r0
	add	ni,r0
	jsr	pc,space
	rts	pc
1:
	mov	lnumber,r1
	sxt	r0
	mov	ndf,-(sp)
	dvd	(sp)+,r0
	tst	r1
	beq	1f
	inc	lnumber
	br	0b
1:
	clr	r0
	cmp	lnumber,$100.
	bge	1f
	inc	r0
	cmp	lnumber,$10.
	bge	1f
	inc	r0
1:
	add	ni,r0
	jsr	pc,space
	mov	lnumber,r0
	jsr	r5,decimal; putchar
	mov	nms,r0
	jsr	pc,space
	inc	lnumber
2:
	rts	pc

newline:
	mov	$'\n,r0
	jsr	pc,putchar
	tst	op
	beq	1f
	inc	dnl
	rts	pc
1:
	tst	x.5
	beq	1f
	mov	$032,r0
	jsr	pc,putchar
1:
	inc	nl
	cmp	nl,pl
	blo	3f
newln1:
	clr	nl
	clr	hnl
	clr	ejf
	mov	$ilist,ejl
	tst	donef
	beq	2f
	tst	nc
	bne	1f
	tst	wch
	bne	1f
	jmp	done1
1:
	tst	ndone
	jne	done1
	inc	ndone
	clr	donef
	cmp	frame,$stk
	bne	2f
	inc	nflush
2:
	inc	pn
	tst	npn
	beq	1f
	mov	npn,pn
	clr	npn
1:
	cmp	pn,pto
	ble	2f
	jsr	pc,flush
	jmp	place
2:
	tst	stop
	beq	2f
	cmp	pn,pfrom
	blo	2f
	jsr	pc,flush
/	mov	sp,r1
/	sys	signal; 2; 1f
	mov	ttyid,r0
	sys	read; char; 1
1:
/	mov	r1,sp
/	sys	signal; 2; place
2:
/	cmp	numbmod,$1
/	bne	3f
/	mov	$1,lnumber
3:
	clr	trap
	jsr	pc,findnl
	tst	r1
	beq	4f
	mov	[mlist-nlist](r1),r0
	mov	frame,-(sp)
	jsr	pc,cont1
	cmp	(sp)+,frame
	beq	4f
	inc	trap
/	inc	nlflg
	rts	pc
4:
	tst	ejf
	beq	5f
	cmp	ilistp,ejl
	beq	newline
5:
	rts	pc

findnl:
	mov	$nlist,r1
1:
	mov	(r1),r0
	tst	r0
	bpl	2f
	add	pl,r0
	inc	r0
2:
	cmp	nl,r0
	beq	4f
3:
	tst	(r1)+
	cmp	r1,$nliste
	bne	1b
	clr	r1
	rts	pc
4:
	tst	[mlist-nlist](r1)
	beq	3b
	rts	pc

number:
	jsr	pc,skipcont
number1:
	clr	-(sp)
	mov	r1,-(sp)
	mov	r3,-(sp)
	clr	r3
	clr	-(sp)
1:
	jsr	pc,getchar
	cmp	r0,$'+
	beq	2f
	cmp	r0,$'-
	beq	2f
	mov	r0,ch
1:
	jsr	pc,atoi
	beq	3f
	mov	r0,r3
	inc	6(sp)
	br	3f
2:
	mov	r0,(sp)
	br	1b
3:
	tst	6(sp)
	bne	1f
	mov	$1,r3
1:
	mov	(r5)+,r0
	beq	1f
	mov	(r0),r0
1:
	mov	(sp)+,r1
	cmp	r1,$'-
	bne	1f
	sub	r3,r0
	br	2f
1:
	cmp	r1,$'+
	bne	1f
	add	r3,r0
	br	2f
1:
	mov	r3,r0
2:
	mov	(sp)+,r3
	mov	(sp)+,r1
	tst	(sp)+
	rts	r5

eject:
/	tst	ejf
/	bne	2f
	inc	ejf
	mov	r0,ejl
	tst	trap
	bne	2f
	jsr	pc,newline
2:
	rts	pc

storeline:
	cmp	linep,$line+linsiz
	bhis	1f
	movb	r0,*linep
	inc	linep
	jsr	pc,width
	add	r1,ne
	sub	r1,nel
	inc	nc
	rts	pc
1:
	tst	over
	bne	2f
	jsr	r5,stringfl; linemes
2:
	inc	over
	rts	pc
linemes: <Line overflow\n\0>
	.even

getword:
	clr	-(sp)
	clr	-(sp)
	mov	pendw,r2
	bne	5f
	mov	$word,r2
	clr	over
	clr	wne
	clr	wch
	clr	nhyph
	clr	hypedf
	mov	$word,wordp
1:
	jsr	pc,gettchar
	bmi	4f
	cmpb	r0,$'\n
	bne	0f
	clr	wne
	clr	wch
	br	3f
0:
	cmpb	r0,ohc
	bne	2f
	inc	hypedf
	br	1b
2:
	cmpb	$' ,r0
	bne	2f
	jsr	pc,storeword
	br	1b
2:
4:
	mov	r0,-(sp)
	mov	$' ,r0
/	bis	chbits,r0
	jsr	pc,storeword
	tst	spaceflg
	beq	2f
	jsr	pc,storeword
	clr	spaceflg
2:
	mov	(sp)+,r0
2:
	tst	r0
	bmi	0f
	cmpb	r0,$005
	beq	6f
0:
	inc	2(sp)
	jsr	pc,storeword
	bisb	(sp),-1(r2)	/add in hyphen
	clr	(sp)
5:
	jsr	pc,gettchar
	bmi	1f
	cmpb	r0,ohc
	bne	1f
	inc	hypedf
	jsr	pc,gettchar
	mov	$200,(sp)
1:
	tst	r0
	bmi	2b
	cmpb	$' ,r0
	beq	1f
	cmpb	$'\n,r0
	bne	2b
	cmpb	-1(r2),$'.
	beq	0f
	cmpb	-1(r2),$'!
	beq	0f
	cmpb	-1(r2),$'?
	bne	1f
0:
	inc	spaceflg
1:
	add	$2,4(sp)
	clrb	(r2)+
3:
	clr	pendw
	cmp	(sp)+,(sp)+
	mov	$word,wordp
	jsr	pc,setnel
/	jsr	pc,wordout
	rts	pc
6:
	tst	(sp)+
	tst	(sp)+
	beq	7f
	mov	r2,pendw
0:
	clr	nflush
	jsr	pc,flushi
	rts	pc
7:
	clr	wch
	br	0b

setnel:
	tst	nc
	bne	2f
	mov	$line,linep
	mov	ll,nel
	tst	un1
	blt	1f
	mov	un1,un
	mov	$-1,un1
1:
	sub	un,nel
	clr	ne
	clr	fac
	clr	fmq
2:
	rts	pc

storeword:
	cmp	r2,$eword
	bhis	1f
	jsr	pc,width
	add	r1,wne
	inc	wch
	movb	r0,(r2)+
	rts	pc
1:
	tst	over
	bne	2f
	jsr	r5,stringfl; wordmes
2:
	inc	over
	rts	pc
wordmes: <Word overflow\n\0>
	.even

need:
need2:
	mov	r0,-(sp)
	clr	r0
	jsr	pc,findt
	cmp	(sp)+,r1
	ble	1f
/	mov	ilistp,r0
/	jsr	pc,eject
	mov	r1,r0
	clr	nls
	jsr	r5,nlines;newline
1:
	rts	pc

min:
	tst	r0
	bge	1f
	clr	r0
1:
	rts	pc

getname:
	mov	r1,-(sp)
	mov	r2,-(sp)
	mov	(r5),r1
	mov	$namesiz,r2
1:
	jsr	pc,getchar
	tst	nlflg
	bne	2f
	cmp	r0,$040
	beq	2f
	cmp	r0,$0176
	blos	4f
2:
	mov	r0,ch
3:
	clrb	(r1)+
	mov	(sp)+,r2
	mov	(sp)+,r1
	tstb	*(r5)+
	rts	r5
4:
	movb	r0,(r1)+
	dec	r2
	beq	3b
	br	1b

copyb:
	inc	copyf
	jsr	pc,flushi
	clr	nlflg
	clr	-(sp)
	mov	$1,-(sp)
1:
	jsr	pc,getchar
	bmi	9f
	cmpb	r0,$'\n
	bne	2f
	mov	$1,(sp)
	clr	nlflg
	br	4f
2:
	cmpb	r0,$'.
	bne	9f
	cmp	(sp),$1
	bgt	3f
	blt	9f
	inc	(sp)
	br	4f
3:
	tst	skp
	bne	0f
	jsr	pc,wbfl
	mov	2(sp),r1
	clr	r0
	jsr	pc,wbt
0:
	inc	(sp)
	br	5f
9:
	clr	(sp)
4:
	mov	r1,2(sp)
	tst	skp
	bne	5f
	jsr	pc,wbf
5:
	cmp	(sp),$3
	bne	1b
	cmp	(sp)+,(sp)+
6:
	clr	copyf
	rts	pc

allocmes: <Out of temp file space.\n\0>
	.even

alloc:
	mov	r1,-(sp)
	clr	nextb
	mov	$blist,r1
1:
	tst	(r1)+
	beq	3f
	cmp	r1,$eblist
	blo	1b
2:
	mov	(sp)+,r1
	tst	nextb
	rts	pc
3:
	mov	$-1,-(r1)
	jsr	pc,offset
	cmp	r1,first
	blo	2b
	mov	r1,nextb
	br	2b

free:
	mov	r1,-(sp)
	mov	r0,r1
	beq	2f
1:
	jsr	pc,blistptr
	tst	(r1)
	beq	2f
	cmp	(r1),$-1
	bne	3f
	clr	(r1)
2:
	mov	(sp)+,r1
	rts	pc
3:
	mov	(r1),-(sp)
	clr	(r1)
	mov	(sp)+,r1
	br	1b

offset:
	sub	$blist,r1
	ash	$7,r1
	add	first,r1
	rts	pc

blistptr:
	mov	r0,-(sp)
	clr	r0
	sub	first,r1
	ashc	$-7,r0
	bic	$1,r1
	add	$blist,r1
	mov	(sp)+,r0
	rts	pc

wbt:
	jsr	pc,wbf
	jsr	pc,wbfl
	rts	pc
wbf:
	tst	woff
	bne	0f
	mov	r1,woff
	mov	$wbuf,wbufp
0:
	mov	r0,*wbufp
	add	$2,wbufp
	add	$2,r1
	bit	$377,r1
	bne	2f
	sub	$2,r1
	jsr	pc,blistptr
	cmp	(r1),$-1
	bne	1f
	jsr	pc,wbfl
	jsr	pc,alloc
	bne	0f
	jsr	r5,stringfl;allocmes
	jmp	done
0:
	mov	nextb,(r1)
1:
	mov	(r1),r1
2:
	cmp	wbufp,$wbufe
	bhis	wbfl
	rts	pc
wbfl:
	tst	woff
	beq	0f
	mov	wbufp,9f
	sub	$wbuf,9f
	beq	0f
	mov	ibf,r0
	sys	0;7f
.data
7:	sys	seek; woff:..;0
.text
	mov	ibf,r0
	sys	0;7f
.data
7:	sys	write; wbuf; 9:..
.text
	clr	woff
	mov	$-1,roff
0:
	rts	pc
rbf:
	mov	ip,r1
	jsr	pc,rbf0
	bne	0f
	tst	app
	bne	1f
	jsr	pc,popi
	rts	pc
0:
	jsr	pc,incoff
1:
	mov	r1,ip
	rts	pc

rbf0:
	mov	r1,-(sp)
	bic	$377,r1
	cmp	r1,roff
	beq	1f
	mov	r1,roff
	mov	ibf1,r0
	sys	0;7f
.data
7:	sys	seek; roff:-1 ;0
.text
	mov	ibf1,r0
	sys	read;rbuf;256.
	tst	r0
	bne	1f
	tst	(sp)+
	sez
	rts	pc
1:
	mov	(sp),r0
	bic	$!377,r0
	mov	(sp)+,r1
	mov	rbuf(r0),r0
	rts	pc

incoff:
	add	$2,r1
	bit	$377,r1
	bne	1f
	sub	$2,r1
	jsr	pc,blistptr
	mov	(r1),r1
	beq	5f
	cmp	$-1,r1
	beq	5f
1:
	rts	pc
5:
	jsr	r5,stringfl; badmes
	jmp	place
badmes: <Bad storage allocation\n\0>
	.even

alph:
	movb	(r0),r2
alph2:
	cmp	r2,$'A
	blo	1f
	cmp	r2,$'Z
	blos	2f
	cmp	r2,$'a
	blo	1f
	cmp	r2,$'z
	bhi	1f
2:
	sez
	rts	pc
1:
	clz
	rts	pc
rdsufb:
	mov	r1,-(sp)
	bic	$177,r1
	cmp	r1,sufoff
	beq	2f
	mov	r1,sufoff
	mov	suff,r0
	sys	0;7f
.data
7:	sys	seek; sufoff: -1; 0
.text
	mov	suff,r0
	sys	read; sufbuf; 128.
2:
	mov	(sp),r0
	bic	$!177,r0
	movb	sufbuf(r0),r0
	mov	(sp)+,r1
	rts	pc


atoi:
	jsr	pc,atoi1
	bne	1f
	rts	pc
1:
	mov	r1,-(sp)
	mov	r0,-(sp)
1:
	jsr	pc,getchar
	cmp	r0,$'+
	beq	4f
	cmp	r0,$'-
	beq	5f
2:
	cmp	r0,$'*
	bne	2f
	jsr	pc,atoi1
	beq	3f
	mov	r0,r1
	mpy	(sp),r1
	mov	r1,(sp)
	br	1b
2:
	cmp	r0,$'\/
	bne	2f
	jsr	pc,atoi1
	beq	3f
	mov	r0,-(sp)
	mov	2(sp),r1
	sxt	r0
	dvd	(sp),r0
	mov	r0,2(sp)
	tst	(sp)+
	br	1b
2:
	mov	r0,ch
3:
	mov	(sp)+,r0
	mov	(sp)+,r1
	tst	pc
	rts	pc
4:
	jsr	pc,atoi1
	beq	3b
	add	r0,(sp)
	br	1b
5:
	jsr	pc,atoi1
	beq	3b
	sub	r0,(sp)
	br	1b

atoi1:
	clr	-(sp)
	mov	r3,-(sp)
	clr	-(sp)
	clr	r3
	jsr	pc,getchar
	cmp	r0,$'-
	bne	2f
	inc	(sp)
1:
	jsr	pc,getchar
2:
	sub	$'0,r0
	cmp	r0,$9
	bhi	1f
	inc	4(sp)
	mpy	$10.,r3
	add	r0,r3
	br	1b
1:
	add	$'0,r0
	mov	r0,ch
	bne	0f
	mov	$' ,ch
0:
	tst	(sp)+
	beq	1f
	neg	r3
1:
	mov	r3,r0
	mov	(sp)+,r3
	tst	(sp)+
	rts	pc

findt:
	mov	r0,-(sp)
	mov	$-1,-(sp)
	mov	$nlist,r1
1:
	tst	[mlist-nlist](r1)
	bne	3f
2:
	tst	(r1)+
	cmp	r1,$nliste
	ble	1b
	br	8f
3:
	mov	(r1),r0
	tst	2(sp)
	bmi	6f	/- traps
	beq	4f	/all traps
	tst	(r1)	/+ traps
	bmi	2b
4:
	tst	(r1)
	bpl	5f
	mov	pl,r0
	inc	r0
	add	(r1),r0
5:
	sub	nl,r0
	ble	2b
	cmp	r0,(sp)
	bhis	2b
	mov	r0,(sp)
	br	2b
6:
	tst	(r1)
	bpl	2b
	br	4b
8:
	mov	(sp),r1
	bpl	9f
	mov	pl,r1
	sub	nl,r1
9:
	cmp	(sp)+,(sp)+
	rts	pc

findr:
	mov	$rlist,r1
1:
	tst	(r1)
	beq	2f
	cmp	(r1)+,r0
	beq	3f
	cmp	r1,$rliste
	blos	1b
	tst	numerr
	bne	0f
	jsr	r5,stringfl; 9f
0:
	inc	numerr
/	clr	r1
/	rts	pc
	cmp	numerr,$1
	jeq	edone
	jmp	done2
2:
	mov	r0,(r1)
	br	4f
3:
	tst	-(r1)
4:
	add	$[vlist-rlist],r1
	rts	pc
9: <No more number registers.\n\0>
	.even

setn0:
	clr	-(sp)
	clr	-(sp)
	mov	$1,nform
	jbr	setn1
setn:
	mov	r1,-(sp)
	clr	-(sp)
	clr	temp
	jsr	pc,get1
	cmpb	r0,$'+
	bne	1f
	inc	(sp)
0:
	jsr	pc,get1
1:
	cmpb	r0,$'\\
	bne	1f
3:
	jsr	pc,get1
	jsr	r5,switch;esctab
	cmpb	r0,dolc
	bne	1f
	jsr	pc,seta
	br	0b
1:
	tst	temp
	bne	2f
	bic	$!177,r0
	cmpb	r0,$'(
	bne	1f
	inc	temp
	jsr	pc,get1
2:
	bic	$!177,r0
	cmpb	r0,$'\\
	beq	3b
	mov	r0,-(sp)
	jsr	pc,get1
	bic	$!177,r0
	swab	r0
	bis	(sp)+,r0
1:
	cmpb	2(sp),$372
	beq	5f /exit if called by \k
	clr	nform
	cmp	r0,$".v
	bne	0f
	mov	ls,r0
	br	setn1
0:
	cmp	r0,$".p
	bne	0f
	mov	pl,r0
	br	setn1
0:
	cmp	r0,$".t
	bne	0f
	clr	r0
	jsr	pc,findt
	mov	r1,r0
	br	setn1
0:
	cmp	r0,$".o
	bne	0f
	mov	po,r0
	br	setn1
0:
	cmp	r0,$".l
	bne	0f
	mov	ll,r0
	br	setn1
0:
	cmp	r0,$".i
	bne	0f
	mov	in,r0
	br	setn1
0:
	cmp	r0,$".$
	bne	0f
	mov	*frame,r0
	br	setn1
0:
	cmp	r0,$".x
	bne	0f
	mov	evp,r0
	br	setn1
0:
	cmp	r0,$".c
	bne	0f
	mov	iline,r0
	br	setn1
0:
	cmp	r0,$".h
	bne	0f
	mov	hnl,r0
	br	setn1
0:
	cmp	r0,$".n
	bne	0f
	mov	lastl,r0
	br	setn1
0:
	jsr	pc,findr
	tst	r1
	beq	5f
	tst	(sp)
	beq	1f
	add	[inclist-vlist](r1),(r1)
1:
	mov	(r1),r0
	mov	[flist-vlist](r1),nform
setn1:
	mov	r4,-(sp)
	mov	$cbuf,r4
	tst	r0
	bge	1f
	movb	$'-,(r4)+
	neg	r0
1:
	jsr	r5,fnumb0;wrc
	clrb	(r4)
	mov	(sp)+,r4
	mov	$cbuf,cp
5:
	tst	(sp)+
	mov	(sp)+,r1
	rts	pc

wrc:
	cmp	r4,$stk
	bhis	1f
	movb	r0,(r4)+
1:
	rts	pc

seta:
	jsr	pc,get1
	cmp	r0,$'\\
	bne	1f
	jsr	pc,get1
	jsr	r5,switch;esctab
	cmp	r0,numc
	bne	1f
	clr	r1
	jsr	pc,setn
	br	seta
1:
	sub	$060,r0
	ble	2f
	cmp	r0,$9.
	bgt	2f
	cmp	r0,*frame
	bgt	2f
	asl	r0
	add	frame,r0
	add	$stksiz-2,r0
	mov	(r0),ap
2:
	rts	pc

	stksiz = 16.
pushi:
	clr	r0
	mov	enda,-(sp)
	sub	$stksiz,(sp)
	cmp	nxf,(sp)+
	blo	0f
	jsr	pc,setbrk
	beq	2f
	br	pushi
0:
	mov	nxf,r0
	tst	(r0)+  /nargs
	mov	frame,(r0)+
	mov	ip,(r0)+
	mov	nspace,(r0)+
	clr	nspace
	mov	rchar,(r0)+
	clr	rchar
	mov	pendt,(r0)+
	mov	ap,(r0)+
	mov	ch,(r0)+
	clr	ch
	clr	ap
	clr	pendt
	mov	nxf,frame
	tst	*nxf
	bne	1f
	add	$stksiz,nxf
	br	2f
1:
	mov	r1,nxf
2:
	tst	r0
	rts	pc

popi:
	cmp	frame,$stk
	beq	1f
	mov	frame,r0
	mov	r0,nxf
	clr	(r0)+
	mov	(r0)+,frame
	mov	(r0)+,ip
	mov	(r0)+,nspace
	mov	(r0)+,rchar
	mov	(r0)+,pendt
	mov	(r0)+,ap
	mov	(r0)+,r0
/	cmp	frame,litlev
/	bhis	1f
/	clr	lit
1:
	rts	pc

setbrk:
	tst	noset
	bne	2f
	mov	enda,-(sp)
	add	$516.,(sp)
	bic	$777,(sp)
	mov	(sp)+,0f
	sys	0;7f
.data
7:	sys	break; 0:..
.text
	bes	1f
	mov	0b,enda
	sub	$2,enda
	clz
	rts	pc
1:
	inc	noset
2:
	sez
	rts	pc

collect:
	inc	copyf
	jsr	pc,skipcont
	clr	*nxf
	mov	nxf,r2
	add	$stksiz,r2
	mov	r2,r1
	add	$18.,r1
	mov	r1,-(sp)
	mov	nxf,-(sp)
	cmp	r1,enda
	blo	1f
	jsr	pc,setbrk
	beq	7f
1:
	clr	quote
	cmp	r2,2(sp)
	beq	9f
	jsr	pc,skipcont
	tst	nlflg
	bne	9f
	mov	r1,(r2)+
	jsr	pc,getchar
	cmp	r0,$'"
	bne	3f
	inc	quote
2:
	jsr	pc,getchar
3:
	tst	nlflg
	bne	8f
	tst	quote
	bne	4f
	cmp	r0,$' /
	beq	8f
	br	5f
4:
	cmp	r0,$'"
	bne	5f
	jsr	pc,getchar
	cmpb	r0,$'"
	bne	8f
5:
	movb	r0,(r1)+
	mov	enda,-(sp)
	sub	$4,(sp)
	cmp	r1,(sp)+
	blo	2b
	jsr	pc,setbrk
	beq	6f
	br	2b
8:
	mov	r0,ch
6:
	clrb	(r1)+
	tst	nlflg
	bne	9f
	tst	noset
	beq	1b
9:
	mov	(sp),nxf
	sub	nxf,r2
	sub	$stksiz,r2
	asr	r2
	mov	r2,*nxf
	bit	$1,r1
	beq	7f
	inc	r1
7:
	clr	copyf
	cmp	(sp)+,(sp)+
	rts	pc
