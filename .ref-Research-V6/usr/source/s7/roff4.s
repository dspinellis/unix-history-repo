/
/

/ roff4 -- runoff

text:
/	inc	tottext
	clr	ulstate
	clr	wch
	clr	wne
	tst	ce
	bne	nofill
	tst	fi
	beq	nofill
	jsr	pc,getchar
2:
	mov	r0,ch
	cmp	$' ,r0
	bne	2f
	jsr	pc,rbreak
1:
	jsr	pc,getchar
	cmp	$' ,r0
	bne	2b
	inc	un
	br	1b
2:
	cmp	r0,$'\n
	bne	2f
	jsr	pc,rbreak
	clr	ch
	jsr	pc,nline
	br	4f
2:
	tst	wch
	bne	3f
	jsr	pc,getword
		br 4f
3:
	jsr	pc,movword
	bne	2b
	jsr	pc,adjust
	br	2b
4:
	dec	ul
	bge	1f
	clr	ul
1:
	rts	pc

nofill:
	jsr	pc,rbreak
1:
	jsr	pc,gettchar
	cmp	r0,$'\n
	beq	1f
	jsr	pc,width
	add	r1,ne
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
	tst	numbmod
	beq	2f
	add	$2,un
2:
	clr	fac
	clr	fmq
	mov	$1000,nwd
	mov	$' ,r0
	jsr	pc,storeline
	jsr	pc,rbreak
	dec	ul
	bpl	2f
	clr	ul
2:
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
	jsr	pc,rbreak
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
	mov	wch,wordend
	mov	wordp,r4
	add	r4,wordend
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
	cmp	nel,$4
	ble	1f
	mov	ls,r0
	add	nl,r0
	cmp	r0,bl
	bgt	2f
	mov	ls,r0
	asl	r0
	add	nl,r0
	cmp	r0,bl
	bgt	1f
2:
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
	sub	$4,r3
	cmp	r3,$word
	blo	2f
	movb	(r3),r2
	bic	$!177,r2
	jsr	pc,alph2
	beq	3f
	cmp	nel,$2
	ble	2f
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
	cmp	r4,$word
	bge	4f
	4
4:
	tst	(sp)+
	sez
	rts	pc

topbot:
	mov	pl,r0
	bne	1f
	clr	bl
	rts	pc
1:
	sub	ma3,r0
	sub	ma4,r0
	sub	hx,r0
	mov	r0,bl
	mov	ma1,r0
	add	ma2,r0
	add	hx,r0
	cmp	r0,bl
	blt	1f
	mov	$2,r0
	mov	r0,ma1
	mov	r0,ma2
	mov	r0,ma3
	mov	r0,ma4
	mov	$66.,pl
	br	topbot
1:
	cmp	nl,bl
	ble	1f
	mov	bl,nl
1:
	rts	pc

width:
	cmp	r0,ohc
	beq	2f
	tst	r0
	beq	2f
	cmp	r0,$0177
	beq	2f
	cmp	r0,$010
	bne	1f
	mov	$-1,r1
	rts	pc
1:
	cmp	$' ,r0
	bgt	2f
	mov	$1,r1
	rts	pc
2:
	clr	r1
	rts	pc

headin:
	jsr	pc,skipcont
	mov	nextb,r1
	mov	r1,*(r5)+
	jsr	pc,gettchar
	cmp	r0,$'\n
	beq	2f
	mov	r0,r2
1:
	jsr	pc,gettchar
	cmp	r0,$'\n
	beq	2f
	cmp	r0,r2
	bne	3f
	clr	r0
3:
	jsr	pc,wbf
	br	1b
2:
	clr	r0
	jsr	pc,wbf
	mov	r1,nextb
	mov	ll,llh
	rts	r5

headout:
	tst	hx
	bne	0f
	tst	(r5)+
	rts	r5
0:
	clr	-(sp)
	mov	*(r5),r2
	mov	ibf1,nfile
	jsr	r5,headseg; width
	mov	r0,-(sp)
	jsr	r5,headseg; width
	mov	r0,-(sp)
	jsr	r5,headseg; width
	mov	r0,-(sp)
	mov	po,r0
	jsr	pc,space
	tst	numbmod
	beq	1f
	mov	$5,r0
	add	ni,r0
	mov	r0,6(sp)
1:
	mov	*(r5)+,r2
	jsr	r5,headseg; putchar
	mov	llh,r0
	add	6(sp),r0
	sub	2(sp),r0
	asr	r0
	sub	4(sp),r0
	bge	1f
	clr	r0
1:
	mov	r0,-(sp)
	jsr	pc,space
	jsr	r5,headseg; putchar
	mov	llh,r0
	sub	(sp)+,r0
	sub	(sp)+,r0
	sub	(sp)+,r0
	sub	(sp)+,r0
	add	(sp)+,r0
	jsr	pc,space
	jsr	r5,headseg; putchar
	jsr	pc,newline
	rts	r5

headseg:
	clr	-(sp)
1:
	mov	r1,-(sp)
	mov	r2,r1
	inc	r2
	jsr	pc,rdsufb
	mov	(sp)+,r1
	tstb	r0
	beq	1f
	cmp	r0,$'%
	beq	2f
	jsr	pc,*(r5)
	add	r1,(sp)
	br	1b
2:
	mov	pn,r0
	clr	r1
	tst	ro
	beq	2f
	mov	$ones,onesp
	mov	$fives,fivesp
	jsr	pc,roman
	add	r1,(sp)
	br	1b
2:
	jsr	pc,decml
	add	r1,(sp)
	br	1b
1:
	mov	(sp)+,r0
	tst	(r5)+
	rts	r5

space:
	jsr	r5,nlines; putchar
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
	jsr	pc,decml
	mov	r1,2(sp)
1:
	mov	(sp)+,r0
	add	$'0,r0
	jsr	pc,*(r5)
	add	(sp)+,r1
	rts	pc

roman:
	mov	r2,-(sp)
	mov	r3,-(sp)
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
	jsr	pc,roman
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

