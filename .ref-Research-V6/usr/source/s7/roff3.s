/
/

/roff3 -- runoff

skipcont:
	jsr	pc,getchar
	mov	r0,r2
	jsr	pc,alph2
	beq	skipcont
1:
	cmp	$' ,r0
	bne	1f
	jsr	pc,getchar
	br	1b
1:
	mov	r0,ch
	rts	pc

rbreak:
	tst	nc
	ble	4f
	clrb	*linep
	inc	totout
	mov	ls,r0
	dec	r0
	jsr	r5,nlines; nline
	tst	pl
	beq	4f
	cmp	nl,bl
	bne	1f
3:
	jsr	pc,eject
1:
	tst	nl
	bne	3f
	mov	ma1,r0
	jsr	r5,nlines; newline
	bit	$1,pn
	bne	1f
	jsr	r5,headout; ehead
	br	2f
1:
	jsr	r5,headout; ohead
2:
	mov	ma2,r0
	jsr	r5,nlines; newline
	dec	skip
	bge	3b
3:
	mov	po,r0
	jsr	pc,space
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
	jsr	pc,newline
	clr	nwd
	clr	ne
	mov	in,un
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
	mov	$5,r0
	add	ni,r0
	jsr	pc,space
	rts	pc
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
	mov	$2,r0
	jsr	pc,space
	inc	lnumber
2:
	rts	pc


newline:
	mov	$'\n,r0
	jsr	pc,putchar
	inc	nl
	rts	pc

nline:
	mov	nl,r0
	beq	1f
	cmp	r0,bl
	beq	1f
	jsr	pc,newline
1:
	rts	pc

number:
	jsr	pc,skipcont
number1:
	mov	r1,-(sp)
	mov	r3,-(sp)
	clr	r3
	clr	-(sp)
	clr	-(sp)
1:
	jsr	pc,getchar
	cmp	r0,$'+
	beq	2f
	cmp	r0,$'-
	beq	2f
	sub	$'0,r0
	cmp	r0,$9.
	bhi	3f
	inc	(sp)
	mpy	$10.,r3
	add	r0,r3
	br	1b
2:
	mov	r0,2(sp)
	br	1b
3:
	add	$'0,r0
	mov	r0,ch
	mov	(sp)+,r0
	bne	1f
	mov	$1,r3
	mov	r3,r0
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
	rts	r5

eject:
	tst	pl
	beq	1f
	tst	nl
	beq	1f
	mov	pl,r0
	sub	nl,r0
	sub	ma4,r0
	sub	hx,r0
	jsr	r5,nlines; newline
	bit	$1,pn
	bne	2f
	jsr	r5,headout; efoot
	br	3f
2:
	jsr	r5,headout; ofoot
3:
	cmp	numbmod,$1
	bne	3f
	mov	$1,lnumber
3:
	mov	ma4,r0
	jsr	r5,nlines; newline
	clr	nl
	inc	pn
1:
	cmp	pn,pto
	ble	1f
	jsr	pc,flush
	jmp	place
1:
istop:
	tst	stop
	beq	2f
	cmp	pn,pfrom
	blo	2f
	jsr	pc,flush
/	mov	sp,r1
/	sys	signal; 2; 1f
	clr	r0
	sys	read; garb; 1
1:
/	mov	r1,sp
/	sys	signal; 2; place
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
1:
	rts	pc

getword:
	mov	$word,r2
	clr	wne
	clr	wch
	clr	nhyph
	clr	hypedf
	mov	$word,wordp
	clr	-(sp)
1:
	jsr	pc,gettchar
	cmp	r0,$'\n
	beq	3f
	cmp	r0,ohc
	bne	2f
	inc	hypedf
	br	1b
2:
	cmp	$' ,r0
	bne	2f
	jsr	pc,storeword
	br	1b
2:
	mov	r0,-(sp)
	mov	$' ,r0
	jsr	pc,storeword
	tst	spaceflg
	beq	2f
	jsr	pc,storeword
	clr	spaceflg
2:
	mov	(sp)+,r0
2:
	jsr	pc,storeword
	bisb	(sp),-1(r2)	/add in hyphen
	clr	(sp)
	jsr	pc,gettchar
	cmp	r0,ohc
	bne	1f
	inc	hypedf
	jsr	pc,gettchar
	mov	$200,(sp)
1:
	cmp	$' ,r0
	beq	1f
	cmp	$'\n,r0
	bne	2b
	cmpb	-1(r2),$'.
	bne	1f
	inc	spaceflg
1:
	add	$2,2(sp)
1:
	clrb	(r2)+
3:
	tst	(sp)+
	mov	$word,wordp
	tst	nc
	bne	1f
	jsr	pc,setnel
1:
	rts	pc

setnel:
	mov	$line,linep
	mov	ll,nel
	sub	un,nel
	clr	ne
	clr	fac
	clr	fmq
	rts	pc

storeword:
	jsr	pc,width
	add	r1,wne
	inc	wch
	movb	r0,(r2)+
	rts	pc

need:
	mov	r0,r3
	mpy	ls,r3
	mov	r3,r0
need2:
	add	nl,r0
	cmp	r0,bl
	ble	1f
	jsr	pc,eject
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
	mov	(r5)+,r1
	mov	$18.,r2
1:
	jsr	pc,getchar
	cmp	r0,$041
	blt	2f
	cmp	r0,$0176
	ble	4f
2:
	mov	r0,ch
3:
	clrb	(r1)+
	mov	(sp)+,r2
	mov	(sp)+,r1
	rts	r5
4:
	movb	r0,(r1)+
	dec	r2
	beq	3b
	br	1b

copyb:
	mov	(r1),r1
	jsr	pc,flushi
	clr	nlflg
	mov	$1,-(sp)
1:
	jsr	pc,getchar
	cmp	r0,$'\n
	bne	2f
	mov	$1,(sp)
	clr	nlflg
	br	4f
2:
	cmp	r0,$'.
	bne	9f
	cmp	(sp),$1
	bgt	3f
	blt	9f
	inc	(sp)
	br	4f
3:
	dec	r1
	clr	r0
	inc	(sp)
	br	4f
9:
	clr	(sp)
4:
	tst	skp
	bne	5f
	jsr	pc,wbf
5:
	cmp	(sp),$3
	bne	1b
	tst	(sp)+
	tst	skp
	bne	6f
	mov	r1,nextb
6:
	rts	pc

popi:
	cmp	ilistp,$ilist
	beq	1f
	sub	$2,ilistp
	mov	*ilistp,ip
1:
	rts	pc

wbf:
	mov	r0,char
	mov	r1,offb
	mov	ibf,r0
	sys	seek; offb:..;0
	mov	ibf,r0
	sys	write; char;1
	inc	r1
	cmp	ibf1,ofile
	bne	1f
	mov	$-1,ofile
1:
	rts	pc
rbf:
	mov	ip,r1
	mov	ibf1,nfile
	jsr	pc,rdsufb
	tstb	r0
	bne	2f
	jsr	pc,popi
	rts	pc
2:
	inc	ip
	rts	pc

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
	bic	$77,r1
	cmp	r1,sufoff
	bne	1f
	cmp	nfile,ofile
	beq	2f
1:
	mov	r1,sufoff
	mov	nfile,ofile
	mov	nfile,r0
	sys	seek; sufoff: -1; 0
	mov	nfile,r0
	sys	read; sufbuf; 512.
2:
	mov	(sp),r0
	bic	$!77,r0
	movb	sufbuf(r0),r0
	mov	(sp)+,r1
	rts	pc
