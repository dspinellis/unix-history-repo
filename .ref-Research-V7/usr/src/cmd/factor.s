.globl	sqrt
exit = 1.
read = 3.
write = 4.
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
	movf	fr0,n
	jsr	pc,sqrt
	movf	fr0,v
	mov	$1,r0
	sys	write; nl; 1
/
	movf	$one,fr0
	movf	fr0,fr4
/
	movf	n,fr0
	movf	$two,fr1
	jsr	r5,xt
/
	movf	n,fr0
	movif	$3,fr1
	jsr	r5,xt
/
	movf	n,fr0
	movif	$5,fr1
	jsr	r5,xt
/
	movf	n,fr0
	movif	$7,fr1
	jsr	r5,xt
/
	movf	n,fr0
	movif	$11.,fr1
	jsr	r5,xt
/
	movf	n,fr0
	movif	$13.,fr1
	jsr	r5,xt
/
	movf	n,fr0
	movif	$17.,fr1
	mov	$tab+6,r4
	jsr	pc,xx
	jmp	begin
/
xt:
	movf	fr0,fr2
	divf	fr1,fr2
	modf	$one,fr2
	movf	fr3,fr2
	mulf	fr1,fr2
	cmpf	fr2,fr0
	cfcc
	beq	hit2
	rts	r5
/
/
out1:
	mov	$tab,r4
	br	in1

out2:
	modf	fr4,fr2
	cfcc
	bne 9f; mov $xx0,-(sp); jmp hit; 9:
	br	in2
xx:
	mov	(r4)+,kazoo
xx0:
	mov	$kazoo,r0
	mov	$100.,r1
	clr	r2
	mov	$gorp,r3
	mov	$gorp+6,r5
xx1:
	movf	fr0,fr2
	divf	fr1,fr2
	cmp	r4,$tabend
	bhis	out1
in1:
	movf	fr2,(r3)
	bit	r2,(r5)
	beq	out2
in2:
kazoo	=.+2
	addf	$kazoo,fr1
	mov	(r4)+,(r0)
	sob	r1,xx1
	mov	$100.,r1
	mov	$127.,r2
	cmpf	v,fr1
	cfcc
	bge	xx1
	cmpf	$one,fr0
	cfcc
	beq	1f
	mov	$1,r0
	sys	write; sp5; 5
	movf	n,fr0
	jsr	r5,ftoa; wrchar
	mov	$1,r0
	sys	write; nl; 1
1:
	rts	pc
/
/
/
hit2:
	movf	fr1,t
	movf	fr3,n
	movf	fr3,fr0
	jsr	pc,sqrt
	movf	fr0,v
	mov	$1,r0
	sys	write; sp5; 5
	movf	t,fr0
	jsr	r5,ftoa; wrchar
	mov	$1,r0
	sys	write; nl; 1
	movf	n,fr0
	movf	t,fr1
	cmp	r4,$tab
	bne	1f
	mov	$tabend,r4
1:
	mov	-(r4),kazoo
	jmp	xt
/
hit:
	movf	fr1,t
	movf	fr3,n
	movf	fr3,fr0
	jsr	pc,sqrt
	movf	fr0,v
	mov	$1,r0
	sys	write; sp5; 5
	movf	t,fr0
	jsr	r5,ftoa; wrchar
	mov	$1,r0
	sys	write; nl; 1
	movf	n,fr0
	movf	t,fr1
	mov	$kazoo,r0
	rts	pc
/
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
/	get one character form the argument string.
getch1:
	movb	(r2)+,r0
	rts	r5
/
/	write one character on the console
/	called from ftoa.
/
wrchar:
	mov	r0,ch
	mov	$1,r0
	sys	write; ch; 1
	rts	r5
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
/
/
ftoa:
	mov	$ebuf,r2
1:
	modf	tenth,fr0
	movf	fr0,fr2
	movf	fr1,fr0
	addf	$epsilon,fr2
	modf	$ten,fr2
	movfi	fr3,r0
	movb	r0,-(r2)
	tstf	fr0
	cfcc
	bne	1b
1:
	movb	(r2)+,r0
	add	$60,r0
	jsr	r5,*(r5)
	cmp	r2,$ebuf
	blo	1b
	tst	(r5)+
	rts	r5
/
epsilon = 037114
tenth:	037314; 146314; 146314; 146315
	.bss
buf:	.=.+18.
ebuf:
	.text
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
ten	= 41040
/
	.data
big:	056177; 177777; 177777; 177777
nl:	<\n>
sp5:	<     >
	.even
/
tab:
	41040; 40400; 40600; 40400; 40600; 40700; 40400; 40700
	40600; 40400; 40600; 40700; 40700; 40400; 40700; 40600
	40400; 40700; 40600; 40700; 41000; 40600; 40400; 40600
	40400; 40600; 41000; 40700; 40600; 40700; 40400; 40600
	40700; 40400; 40700; 40700; 40600; 40400; 40600; 40700
	40400; 40700; 40600; 40400; 40600; 40400; 41040; 40400
tabend:
/
	.bss
ch:	.=.+2
t:	.=.+8
n:	.=.+8
v:	.=.+8
gorp:	.=.+8
argflg:	.=.+2
	.text
ldfps = 170100^tst
stfps = 170200^tst
/
/	sqrt replaces the f.p. number in fr0 by its
/	square root.  newton's method
/
.globl	sqrt, _sqrt
/
/
_sqrt:
	mov	r5,-(sp)
	mov	sp,r5
	movf	4(r5),fr0
	jsr	pc,sqrt
	mov	(sp)+,r5
	rts	pc

sqrt:
	tstf	fr0
	cfcc
	bne	1f
	clc
	rts	pc		/sqrt(0)
1:
	bgt	1f
	clrf	fr0
	sec
	rts	pc		/ sqrt(-a)
1:
	mov	r0,-(sp)
	stfps	-(sp)
	mov	(sp),r0
	bic	$!200,r0		/ retain mode
	ldfps	r0
	movf	fr1,-(sp)
	movf	fr2,-(sp)
/
	movf	fr0,fr1
	movf	fr0,-(sp)
	asr	(sp)
	add	$20100,(sp)
	movf	(sp)+,fr0	/initial guess
	mov	$4,r0
1:
	movf	fr1,fr2
	divf	fr0,fr2
	addf	fr2,fr0
	mulf	$half,fr0	/ x = (x+a/x)/2
	sob	r0,1b
2:
	movf	(sp)+,fr2
	movf	(sp)+,fr1
	ldfps	(sp)+
	mov	(sp)+,r0
	clc
	rts	pc
/
half	= 40000
