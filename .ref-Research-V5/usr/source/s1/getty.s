/ getty --  get name and tty mode
/ for initialization

/ cycle through speeds and "login:" messages
/ summarized in itab

stty = 31.
gtty = 32.

	sys	signal; 3; 1
	sys	signal; 2; 1
	clr	r0
	sys	gtty; imode
	mov	imode+4,r0
	bic	$!26,r0
	mov	r0,flags	/ use xtab,cr,ucase from driver
	jsr	r5,nextspeed
1:
	mov	$name,r5
2:
	jsr	r5,getc
	cmp	r0,$'\n
	beq	1f
	cmp	r0,$'\r
	beq	4f
	cmpb	r0,imode+3
	beq	1b
	cmpb	r0,imode+2
	bne	3f
	cmp	r5,$name
	blos	2b
	dec	r5
	br	2b
3:
	cmp	r5,$name+30.
	bhis	2b
	movb	r0,(r5)+
	br	2b
4:
	bis	$20,flags		/cr bit
	mov	$'\n,r0
	jsr	pc,putc
	br	2f
1:
	mov	$'\r,r0
	jsr	pc,putc
2:
	clrb	(r5)+

/ determine whether terminal is upper-case only

	mov	$name,r5
1:
	movb	(r5)+,r0
	beq	1f
	cmp	r0,$'A
	blo	2f
	cmp	r0,$'Z
	bhi	2f
	bis	$4,flags
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
	mov	fstate,r4
	mov	imode+2,2(r4)
	bis	flags,4(r4)
	clr	r0
	sys	0; 9f
.data
9:
	sys	stty; fstate: ..
.text

go:
	sys	exec; login; loginp
	sys	exit

getc:
	clr	r0
	sys	read; ch; 1
	bes	done
	tst	r0
	beq	done
	bic	$!177,ch
	mov	ch,r2
	beq	1f
getc1:
	cmp	r2,$174
	bhis	3f
	mov	$1,r0
	movb	ch,r0
	jsr	pc,putc
3:
	mov	r2,r0
	rts	r5
1:
	mov	$name,(sp)
	jsr	r5,nextspeed
2:
	clr	r0		/ flush nulls
	sys	read; ch; 1
	bes	done
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
	sys	0; 9f
.data
9:
	sys	stty; 0:..
.text
	bes	go
	mov	(r1)+,-(sp)
	mov	(r1)+,fstate
	mov	r1,itabp
	mov	(sp)+,r1
1:
	movb	(r1)+,r0
	beq	1f
	jsr	pc,putc
	br	1b
1:
	rts	r5

putc:
	bic	$!177,r0
	movb	r0,ch
	mov	$1,r0
	sys	write; ch; 1
	rts	pc

loginp:	login
	name
	0

itab:
	itn300; tnmes;  tn300
	itty37; ttymes; tty37
	0

.data
itabp:	itab
itty37:
	.byte	5, 5
	0; 340	/ any parity, raw
tty37:
	.byte	5, 5
	0; 210	/ 37 parity, echo
itn300:
	.byte	7, 7
	0; 340	/ any parity, raw, cr
tn300:
	.byte	7, 7
	0; 10310	/ any parity, echo, no tab del

ttymes:
	<\n\r\p:\alogin: \0>
tnmes:
	<\n\r\p;login: \0>

login:	</bin/login\0>
	.even

nl:	<\n>
cr:	<\r>

.bss
flags:	.=.+2
ch:	.=.+2
imode:	.=.+6
name:	.=.+32.
