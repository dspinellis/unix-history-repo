/ db2 -- debugger

lookupn:
	cmp	symbol,$'.
	bne	1f
	mov	dot,taddr
	rts	pc
1:
	tst	error
	beq	1f
	rts	pc
1:
	mov	r2,-(sp)
	mov	namstrt,r1
	mov	namsiz,r2
	jsr	pc,1f
	mov	(sp)+,r2
	rts	pc
1:
	mov	$symbol,r0
	cmp	(r0)+,(r1)+
	bne	2f
	cmp	(r0)+,(r1)+
	bne	3f
	cmp	(r0)+,(r1)+
	bne	4f
	cmp	(r0)+,(r1)+
	bne	5f
	tst	(r1)+
	mov	(r1)+,taddr
	rts	pc
2:
	tst	(r1)+
3:
	tst	(r1)+
4:
	tst	(r1)+
5:
	cmp	(r1)+,(r1)+
	cmp	r1,r2
	blo	1b
	inc	error
	clr	taddr
	rts	pc

lookupv:
	mov	r5,-(sp)
	mov	$nambuf,r5
	clr	r2
	mov	$177777,r3
1:
	cmp	r5,namsiz
	bhis	4f
	mov	12(r5),r1
	sub	r0,r1
	neg	r1
	blt	3f
	cmp	r1,r3
	bhi	3f
	cmp	r1,$4000
	bhis	3f
	cmp	r2,r5
	bhi	3f
	mov	r1,r3
	mov	r5,r2
3:
	add	$14,r5
	br	1b
4:
	mov	(sp)+,r5
	rts	pc

get:
	mov	*(r5)+,r0
	jsr	r5,remap
	tst	error
	bne	2f
	mov	r0,0f
	mov	curfin,r0
	sys	seek; 0:0; 0
	bec	1f
2:
	inc	error
	rts	r5
1:
	mov	curfin,r0
	sys	read; temp; 2
	bes	2b
	tst	r0
	beq	2b
	mov	temp,r0
	rts	r5

remap:
	mov	dbfin,curfin
	cmp	getoff,$1024.
	bne	1f
	cmp	r0,txtsiz
	bhis	2f
	add	$20,r0
	mov	symfin,curfin
	rts	r5
2:
	cmp	r0,rtxtsiz
	blo	3f
	sub	rtxtsiz,r0
	cmp	r0,datsiz
	bhis	2f
	add	$1024.,r0
	rts	r5
2:
	add	rtxtsiz,r0
	neg	r0
	cmp	r0,stksiz
	bhi	3f
	neg	r0
	add	datsiz,r0
	add	stksiz,r0
	add	$1024.,r0
	rts	r5
1:
	add	getoff,r0
	rts	r5
3:
	inc	error
	rts	r5

printo:
	mov	$obuf+6,r1
	clr	r2
1:
	inc	r2
	movb	r0,-(r1)
	bicb	$!7,(r1)
	bisb	$'0,(r1)
	clc
	ror	r0
	asr	r0
	asr	r0
	bne	1b
	mov	r2,0f+2
	mov	r1,0f
	mov	$1,r0
	sys	write; 0:obuf; 6
	rts	pc

mesg:
	movb	(r5)+,r0
	beq	1f
	jsr	pc,putc
	br	mesg
1:
	inc	r5
	bic	$1,r5
	rts	r5

pnl:
	jsr	r5,mesg; <\n\0>
	rts	pc

psp:
	jsr	r5,mesg; < \0>
	rts	pc

pstar:
	jsr	r5,mesg; <*\0>
	rts	pc

plp:
	jsr	r5,mesg; <(\0>
	rts	pc

prp:
	jsr	r5,mesg; <)\0>
	rts	pc

pb:
	jsr	r5,mesg; <b\0>
	rts	pc

pcom:
	jsr	r5,mesg; <,\0>
	rts	pc

put:
	mov	*(r5)+,r0
	jsr	r5,remap
	tst	error
	bne	2f
	cmp	curfin,dbfin
	bne	2f
	mov	r0,0f
	mov	dbfout,r0
	bne	1f
2:
	tst	(r5)+
3:
	inc	error
	rts	r5
1:
	tst	error
	bne	3b
	sys	seek; 0:0; 0
	bes	2b
	mov	(r5)+,0f
	mov	dbfout,r0
	tst	bytemod
	beq	1f
	mov	$1,0f+2
	br	2f
1:
	mov	$2,0f+2
2:
	sys	write; 0:0; 2
	bes	3b
	rts	r5

decodadr:
	mov	r0,r3
	mov	r3,-(sp)
	bic	$!10,(sp)
	bic	$!7,r0
	cmp	r0,$7
	beq	pcadr
7:
	mov	r3,r0
	asr	r0
	asr	r0
	asr	r0
	bic	$!6,r0
	jmp	*1f(r0)

1:
	simp
	incr
	decr
	indx

simp:
	tst	(sp)
	beq	1f
	jsr	pc,plp
1:
	jsr	pc,preg
	tst	(sp)+
	beq	9f
	jsr	pc,prp
	br	9f

incr:
	tst	(sp)+
	beq	1f
	jsr	pc,pstar
1:
	jsr	pc,plp
	jsr	pc,preg
	jsr	r5,mesg; <)+\0>; .even
	br	9f

decr:
	tst	(sp)+
	beq	1f
	jsr	pc,pstar
1:
	jsr	r5,mesg; <-(\0>; .even
	jsr	pc,preg
	jsr	pc,prp
	br	9f

indx:
	tst	(sp)+
	beq	1f
	jsr	pc,pstar
1:
	jsr	pc,get1
	jsr	pc,pname
	jsr	pc,plp
	jsr	pc,preg
	jsr	pc,prp
	br	5f

pcadr:
	mov	r3,r0
	bit	$20,r3
	beq	7b
	tst	(sp)+
	beq	1f
	jsr	pc,pstar
1:
	bit	$40,r3
	bne	6f
	jsr	r5,mesg; <$\0>
	jsr	pc,get1
	jsr	pc,pname
	br	5f
6:
	jsr	pc,get1
	add	$2,r0
	add	r1,r0
	mov	$3,r1
	jsr	pc,pname
	br	5f
9:
	tst	(r5)+
	clr	r0
	rts	r5
5:
	mov	$2,r0
	rts	r5

preg:
	mov	r3,r0
	bic	$!7,r0
	asl	r0
	mov	regtab(r0),obuf
	mov	$1,r0
	sys	write; obuf; 2
	rts	pc

regtab:
	"r0
	"r1
	"r2
	"r3
	"r4
	"r5
	"sp
	"pc

pname:
	mov	r5,-(sp)
	mov	r3,-(sp)
	mov	r1,-(sp)
	jsr	pc,lookupv
	tst	r2
	beq	5f
	cmp	(sp),$1
	bne	1f
	tst	r3
	bne	5f
1:
	mov	r2,r1
	mov	$8,-(sp)
	mov	r0,r5
1:
	movb	(r1)+,r0
	beq	1f
	jsr	pc,putc
	dec	(sp)
	bne	1b
1:
	tst	(sp)+
	cmp	12(r2),r5
	beq	1f
	blt	2f
	jsr	r5,mesg; <-\0>
	br	3f
2:
	jsr	r5,mesg; <+\0>
3:
	mov	r3,r0
5:
	jsr	pc,printo
1:
	tst	(sp)+
	mov	(sp)+,r3
	mov	(sp)+,r5
	rts	pc

get1:
	mov	*(r5)+,r1
	add	$2,r1
	mov	r1,temp
	jsr	r5,get; temp
	rts	pc
