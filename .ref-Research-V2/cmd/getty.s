/ getty --  get name and tty mode
/ for initialization

/ cycle through speeds and "login:" messages
/ summarized in itab

stty = 31.

	sys	quit; 0
	sys	intr; 0
0:
	jsr	r5,nextspeed
1:
	mov	$name,r5
2:
	jsr	r5,getc
	cmp	r0,$174
	beq	5f
	cmp	r0,$176
	beq	5f
	cmp	r0,$'\n
	beq	1f
	cmp	r0,$'\r
	beq	4f
	cmp	r0,$'@
	beq	1b
	cmp	r0,$'#
	bne	3f
	cmp	r5,$name
	blos	2b
	dec	r5
	br	2b
3:
	movb	r0,(r5)+
	br	2b
4:
	bis	$20,flags		/cr bit
	mov	$1,r0
	sys	write; nl; 1
	br	2f
5:
	mov	$tab2741,itabp
	inc	nowr
	br	0b
1:
	mov	$1,r0
	sys	write; cr; 1
2:
	clrb	(r5)+

/ determine whether terminal is upper-case only

	cmp	r5,$name+1
	bhi	1f
	bic	$4,flags	/no data-assume lc
1:
	mov	$name,r5
1:
	movb	(r5)+,r0
	beq	1f
	cmp	r0,$'A
	blo	2f
	cmp	r0,$'Z
	bhi	2f
	add	$40,r0		/ map to lc
	movb	r0,-1(r5)
	br	1b
2:
	cmp	r0,$'a
	blo	1b
	cmp	r0,$'z
	bhi	1b
	bic	$4,flags
	br	1b
1:
	clr	r0
	mov	fstate,r4
	bis	flags,4(r4)
	sys	stty; fstate: ..

go:
	sys	exec; login; loginp
	sys	exit

getc:
	clr	r0
	sys	read; ch; 1
	tst	r0
	beq	done
	mov	ch,r2
	beq	1f
getc1:
	cmp	r2,$174
	bhis	3f
	tst	nowr
	bne	3f
	mov	$1,r0
	sys	write; ch; 1
3:
	mov	r2,r0
	rts	r5
1:
	dec	$0		/ wait a while
	bne	1b
	mov	$name,(sp)
	jsr	r5,nextspeed
2:
	clr	r0		/ flush nulls
	sys	read; ch; 1
	tst	r0
	beq	done
	movb	ch,r2
	beq	2b
	br	getc1

done:
	sys	exit

nextspeed:
	mov	itabp,r1
	mov	(r1)+,0f
	bne	1f
	mov	$itab,itabp
	br	nextspeed
1:
	clr	r0
	sys	stty; 0:..
	bes	go
	mov	(r1)+,-(sp)
	mov	(r1)+,fstate
	mov	r1,itabp
	mov	(sp)+,r1
1:
	movb	(r1)+,ch
	beq	1f
	mov	$1,r0
	sys	write; ch; 1
	br	1b
1:
	rts	r5

itabp:	itab
loginp:	login
	name
	0

itab:
	itty37; ttymes; tty37
	itn300; tnmes;  tn300
tab2741: i2741; m2741; f2741
	0

itty37:	511; 511; 340	/ any parity, raw, 150 baud
tty37:	511; 511; 210	/ 37 parity, echo, 150 baud
itn300:	521; 521; 340	/ any parity, raw, cr, 300 baud
tn300:	521; 521; 310	/ any parity, echo, 300 baud
i2741:	1501; 501; 100540	/134 bits, 2741, raw, first time
f2741:	1501; 501; 500	/134 bps, 2741

	0
m2741:	<\nlogin: \0>

ttymes:
	<\n\r\p:\alogin: \0>
tnmes:
	<\n\r\p;login: \0>

login:	</bin/login\0>
	.even

nl:	<\n>
cr:	<\r>

flags:	004	/ upper case map

	.bss
ch:	.=.+2
nowr:	.=.+2
name:	.=.+32.
f2741
	0

itty37:	511; 511; 340	/ any parity, raw, 150 baud
tty37:	511; 511; 210	/ 37 parity, echo, 150 baud
itn300:	521; 521; 340	/ any parity, raw, cr, 300 baud
tn300:	521; 521; 310	/ any parity, echo, 300 baud
i2741:	1501; 501; 100540	/134 bits, 2741, raw, first time
f2741:	1501; 501; 500	/134 bps, 2741

	0
m2741:	<\nlogin: \0>

ttymes:
	<\n\r\p:\alogin: \0>
tnmes:
	<\n\r\p;log