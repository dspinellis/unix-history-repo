ldfps = 170100^tst
/
	ldfps	$240

	clr	argflg
	cmp	(sp)+,$2
	blt	begin
	tst	(sp)+
	mov	(sp),r2
	jsr	r5,atof; getch1
	inc	argflg
	br	begin1
begin:
	tst	argflg
	beq 9f; sys exit; 9:
	jsr	r5,atof; getch
begin1:
	tstf	fr0
	cfcc
	bpl 9f; jmp ouch; 9:
	bne 9f; sys exit; 9:
	cmpf	big,fr0
	cfcc
	bgt 9f; jmp ouch; 9:
/
	movf	$f100,fr1
	cmpf	fr0,fr1
	cfcc
	bge	1f
	mov	$pt,r3
3:
	cmp	r3,$ptend
	bhis	1f
	movif	(r3)+,fr1
	cmpf	fr1,fr0
	cfcc
	blt	3b
	tst	-(r3)
3:
	movif	(r3),fr0
	jsr	r5,ftoa; wrchar
	mov	$'\n,r0
	jsr	r5,wrchar
	tst	(r3)+
	cmp	r3,$ptend
	blo	3b
	movf	$f100,fr0
/
1:
	divf	$two,fr0
	modf	$one,fr0
	movf	fr1,fr0
	mulf	$two,fr0
	addf	$one,fr0
	movif	$tsiz8,fr1
	movf	fr1,fr5
	movf	fr0,nn
/
/
/
/	clear the sieve table
/
2:
	mov	$table,r3
3:
	cmp	r3,$table+tabsiz
	bhis	3f
	clrb	(r3)+
	br	3b
/
/	run the sieve
/
3:
	movf	nn,fr0
	addf	fr5,fr0
	jsr	r5,sqrt
	movf	fr0,v
/
	movf	nn,fr0
	movif	$3.,fr1
	jsr	pc,5f
	movif	$5.,fr1
	jsr	pc,5f
	movif	$7.,fr1
	jsr	pc,5f
	movif	$11.,fr1
	mov	$factab+2,r4
4:
	jsr	pc,5f
	mov	(r4)+,kazoo
kazoo	=.+2
	addf	$kazoo,fr1
	cmp	r4,$ftabend
	blo	3f
	mov	$factab,r4
3:
	cmpf	v,fr1
	cfcc
	bge	4b
	br	1f
/
/
5:
	movf	fr0,fr2
	divf	fr1,fr2
	modf	$one,fr2
	mulf	fr1,fr3
	subf	fr0,fr3
	cfcc
	bpl	3f
	addf	fr1,fr3
3:
	cmpf	fr5,fr3
	cfcc
	ble	3f
	movfi	fr3,r0
	ashc	$-3.,r0
	ash	$-13.,r1
	bic	$177770,r1
	bisb	bittab(r1),table(r0)
	addf	fr1,fr3
	br	3b
3:
	rts	pc
/
/
/	get one character form the argument string.
getch1:
	movb	(r2)+,r0
	rts	r5
/
/	now get the primes from the table
/	and print them.
/
1:
/
	movf	nn,fr0
	clr	r3
	br	4f
/
1:
	inc	r3
	inc	r3
	cmp	r3,$tsiz8
	bge	2b
/
4:
/
	jsr	pc,prime
	bec	3f
	movf	nn,fr0
	jsr	r5,ftoa; wrchar
	mov	$'\n,r0
	jsr	r5,wrchar
3:
	movf	nn,fr0
	addf	$two,fr0
	movf	fr0,nn
	br	1b
/
/
/
/
prime:
	mov	r3,r4
	ashc	$-3.,r4
	ash	$-13.,r5
	bic	$177770,r5
	bitb	bittab(r5),table(r4)
	bne	1f
	sec
1:
	rts	pc
/
/
/
/
one	= 40200
half	= 40000
opower	= 34400
power	= 44000
f100	= 41710
/
/	get one character from the console.
/	called from atof.
/
getch:
	clr	r0
	sys	read; ch; 1
	bec 9f; sys exit; 9:
	tst r0; bne 9f; sys exit; 9:
	mov	ch,r0
	rts	r5
/
/
/	write one character on the console
/	called from ftoa.
/
wrchar:
	tst	iobuf
	bne	1f
	mov	$iobuf+2,iobuf
1:
	movb	r0,*iobuf
	inc	iobuf
	cmp	iobuf,$iobuf+514.
	blo	1f
	mov	$1,r0
	sys	write; iobuf+2; 512.
	mov	$iobuf+2,iobuf
1:
	rts	r5
/
	.bss
iobuf:	.=.+518.
	.text
/
/
/	read and convert a line from the console into fr0.
/
atof:
	mov	r1,-(sp)
	movif	$10.,r3
	clrf	r0
1:
	jsr	r5,*(r5)
	sub	$'0,r0
	cmp	r0,$9.
	bhi	2f
	mulf	r3,r0
	movif	r0,r1
	addf	r1,r0
	br	1b
2:
	cmp	r0,$' -'0
	beq	1b
/
	mov	(sp)+,r1
	tst	(r5)+
	rts	r5
/
/
ftoa:
	mov	$ebuf,r2
1:
	movf	fr0,fr1
	divf	$ten,fr1
	movf	fr1,fr2
	modf	$one,fr2
	movf	fr3,-(sp)
	mulf	$ten,fr3
	negf	fr3
	addf	fr0,fr3
	movfi	fr3,-(r2)
	movf	(sp)+,fr0
	tstf	fr0
	cfcc
	bne	1b
1:
	mov	(r2)+,r0
	add	$60,r0
	jsr	r5,*(r5)
	cmp	r2,$ebuf
	blo	1b
	tst	(r5)+
	rts	r5
/
/
/
/	replace the f.p. number in fr0 by its square root
/
sqrt:
	movf	r0,r1		/ a
	tstf	fr0
	cfcc
	beq	2f
	bgt	1f
	sec
	rts	r5		/ sqrt(-a)
1:
	seti
	movf	fr0,-(sp)
	asr	(sp)
	add	$20100,(sp)
	movf	(sp)+,fr0
	movif	$2,r3		/ constant 2
	mov	$4,r0
1:
	movf	r1,r2
	divf	r0,r2
	addf	r2,r0
	divf	r3,r0		/ x = (x+a/x)/2
	dec	r0
	bgt	1b
2:
	clc
	rts	r5
/
/
buf:	.=.+38.
ebuf:
/
/
/
/	complain about a number which the program
/	is unable to digest
ouch:
	mov	$2,r0
	sys	write; 1f; 2f-1f
	jmp	begin
/
1:	<Ouch.\n>
2:	.even
/
/
one	= 40200
two	= 40400
four	= 40600
six	= 40700
ten	= 41040
/
	.data
bittab:	.byte	1, 2, 4, 10, 20, 40, 100, 200
big:	056177; 177777; 177777; 177777
/
pt:	2.; 3.; 5.; 7.; 11.; 13.; 17.; 19.; 23.; 29.; 31.; 37.; 41.; 43.
	47.; 53.; 59.; 61.; 67.; 71.; 73.; 79.; 83.; 89.; 97.
ptend:
nl:	<\n>
sp5:	<     >
	.even
/
/
factab:
	41040; 40400; 40600; 40400; 40600; 40700; 40400; 40700
	40600; 40400; 40600; 40700; 40700; 40400; 40700; 40600
	40400; 40700; 40600; 40700; 41000; 40600; 40400; 40600
	40400; 40600; 41000; 40700; 40600; 40700; 40400; 40600
	40700; 40400; 40700; 40700; 40600; 40400; 40600; 40700
	40400; 40700; 40600; 40400; 40600; 40400; 41040; 40400
ftabend:
/
	.bss
ch:	.=.+2
t:	.=.+8
n:	.=.+8
v:	.=.+8
nn:	.=.+8
place:	.=.+8
/
tabsiz	= 1000.
tsiz8	= 8000.
table:	.=.+tabsiz
argflg:	.=.+2
	.text
