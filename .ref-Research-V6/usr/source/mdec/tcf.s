/ unix DEC-tape time/mtrack track formatter

nword = 256.
nblock = 578.
endz = 7200.

ps = 177776
tcst = 177340
tccm = 177342
tcwc = 177344
tcba = 177346
tcdt = 177350

	mov	r5,savr5
	mov	sp,savsp

	jsr	pc,4(r5)
		<ready drive 0 and type y\n\0>; .even
	jsr	pc,2(r5)
	mov	r0,-(sp)
	mov	$'\n,r0
	jsr	pc,(r5)
	cmp	(sp)+,$'y
	beq	1f
	rts	pc
1:
	clr	r0
	mov	$cobtab,r1
1:
	jsr	pc,bcomobv
	mov	r2,(r1)+
	inc	r0
	cmp	r0,$512.
	bne	1b

	mov	$buffer,r0
1:
	mov	$-1,(r0)+
	cmp	r0,$buffer+nword+nword
	blo	1b

	mov	savr5,r5
	jsr	pc,2(r5)

	mov	$340,ps
	mov	$tcdt,r5
	mov	$13,tccm
	jsr	pc,flag

/ 8Kch for end zone

	mov	$endz,r4
1:
	jsr	r0,mtrack; 101101
	dec	r4
	bne	1b

/ foreward guard

	mov	$199.,r4
1:
	jsr	r0,mtrack; 10101
	dec	r4
	bne	1b

/ blocks

	mov	$nblock,r3
1:
	jsr	r0,mtrack; 10101
	jsr	r0,mtrack; 10110
	jsr	r0,mtrack; 11010
	jsr	r0,mtrack; 01000
	jsr	r0,mtrack; 01000
	jsr	r0,mtrack; 01000
	jsr	r0,mtrack; 01000
	mov	$nword-4,r4
2:
	jsr	r0,mtrack; 111000
	dec	r4
	bne	2b
	jsr	r0,mtrack; 111011
	jsr	r0,mtrack; 111011
	jsr	r0,mtrack; 111011
	jsr	r0,mtrack; 111011
	jsr	r0,mtrack; 101001
	jsr	r0,mtrack; 100101
	jsr	r0,mtrack; 10101
	dec	r3
	bne	1b

/ rear guard

	mov	$199.,r4
1:
	jsr	r0,mtrack; 10101
	dec	r4
	bne	1b

/ rear end zone

	mov	$endz,r4
1:
	jsr	r0,mtrack; 10010
	dec	r4
	bne	1b
	clr	tccm

/ end of mtrack pass
/ go back 12 blocks

	mov	$12.,r0
	mov	$4002,tccm
1:
	incb	tccm
	jsr	pc,flag
	dec	r0
	bne	1b

/ put comobv(nblock-1) in everything
/ up to end zone

	mov	$nblock-1,r0
	mov	r0,bn
	jsr	pc,comobv
	mov	$17,tccm
	jsr	pc,flag
1:
	movb	r1,tcst
	mov	r0,(r5)
	jsr	pc,flag1
		br 1b

/ reverse pass put in
/ foreward and reverse
/ block numbers

	mov	$4017,tccm
	jsr	pc,flag
1:
	mov	$nword+3.,r4
2:
	clrb	tcst
	clr	(r5)
	jsr	pc,flag
	dec	r4
	bne	2b

	clrb	tcst
	clr	(r5)
	mov	bn,r0
	jsr	pc,comobv
	jsr	pc,flag
	movb	r1,tcst
	mov	r0,(r5)
	jsr	pc,flag
	dec	bn
	blt	check
	clrb	tcst
	clr	(r5)
	jsr	pc,flag
	clrb	tcst
	clr	(r5)
	jsr	pc,flag
	clrb	tcst
	mov	bn,(r5)
	jsr	pc,flag
	clrb	tcst
	clr	(r5)
	jsr	pc,flag
	clrb	tcst
	clr	(r5)
	jsr	pc,flag
	br	1b

/ foreward pass
/ confirm block numbers
/ write all 1's in data

check:
	clrb	tcst
	clr	(r5)
	jsr	pc,flag1
		br check
	clr	r4
1:
	mov	$3,tccm
	jsr	pc,flag
	cmp	(r5),r4
	bne	error1
	mov	$-nword,tcwc
	mov	$buffer,tcba
	mov	$15,tccm
	jsr	pc,flag
	inc	r4
	cmp	r4,$nblock
	bne	1b
	mov	$3,tccm
	jsr	pc,flag1
		br error2

/ reverse pass
/ confirm block numbers
/ read data and compare
/ to all 1's

1:
	mov	$4003,tccm
	jsr	pc,flag
	dec	r4
	cmp	r4,(r5)
	bne	error3
	mov	$-nword,tcwc
	mov	$buffer,tcba
	mov	$4005,tccm
	jsr	pc,compare
	jsr	pc,flag
	tst	r4
	bne	1b
	mov	$4003,tccm
	jsr	pc,flag1
		br error4
	jsr	pc,compare
	mov	savr5,r5
	mov	savsp,sp
	rts	pc

error1:
	mov	$1,r0
	br	1f
error2:
	mov	$1,r0
	br	1f
error3:
	mov	$3,r0
	br	1f
error4:
	mov	$4,r0
	br	1f
error5:
	mov	$5,r0
	br	1f
error6:
	mov	$6,r0
	br	1f
error7:
	mov	$7,r0
1:
	mov	$1,tccm
	mov	savr5,r5
	mov	savsp,sp
	jsr	pc,4(r5)
		<tcf: error\n\0>; .even
	rts	pc

compare:
	mov	r0,-(sp)
	mov	$buffer,r0
1:
	cmp	(r0)+,$-1
	bne	error5
	cmp	r0,$buffer+nword+nword
	blo	1b
	mov	(sp)+,r0
	rts	pc

mtrack:
	mov	(r0)+,(r5)
	jsr	pc,flag
	rts	r0

flag:
	bit	$100200,tccm
	beq	flag
	blt	error6
	rts	pc

flag1:
	bit	$100200,tccm
	beq	flag1
	bge	1f
	tst	tcst
	bge	error7
	add	$2,(sp)
1:
	rts	pc

comobv:
	mov	r0,r4
	bic	$!777,r0
	asl	r0
	mov	cobtab(r0),r0
	swab	r0
	clr	r1
	ror	r0
	rol	r1
	asl	r0
	asl	r0
	rol	r1
	swab	r4
	bic	$177401,r4
	bis	cobtab(r4),r0
	rts	pc

bcomobv:
	mov	r0,r2
	bic	$!70,r2
	mov	r0,r3
	mov	$6.,r4
1:
	asr	r3
	dec	r4
	bne	1b
	bic	$!7,r3
	bis	r3,r2
	mov	r0,r3
	mov	$6.,r4
1:
	asl	r3
	dec	r4
	bne	1b
	bic	$!700,r3
	bis	r3,r2
	com	r2
	bic	$!777,r2
	rts	pc

bn:	.=.+2
savr5:	.=.+2
savsp:	.=.+2
cobtab:	.=.+1024.
buffer:	.=.+nword+nword
