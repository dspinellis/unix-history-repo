/ C library -- printf

.globl	_printf
.globl	fltused, pfloat, pscien, _atof

/.globl	pfloat
/.globl	pscien
.globl	_putchar

.globl	csv
.globl	cret

_atof:
fltused:
pfloat:
pscien:
_printf:
	jsr	r5,csv
	sub	$126.,sp
	mov	4(r5),formp		/ format
	mov	r5,r4
	add	$6,r4			/ arglist
loop:
	movb	*formp,r0
	beq	1f
	inc	formp
	cmp	r0,$'%
	beq	2f
3:
	mov	r0,(sp)
	jsr	pc,*$_putchar
	br	loop
1:
	jmp	cret
2:
/	clr	rjust
	clr	ndigit
/	cmpb	*formp,$'-
/	bne	2f
/	inc	formp
/	inc	rjust
2:
	jsr	r3,gnum
	mov	r1,width
/	clr	ndfnd
	cmp	r0,$'.
	bne	1f
	jsr	r3,gnum
	mov	r1,ndigit
1:
	mov	sp,r3
	add	$4,r3
	mov	$swtab,r1
1:
	mov	(r1)+,r2
	beq	3b
	cmp	r0,(r1)+
	bne	1b
	jmp	(r2)
	.data
swtab:
	decimal;	'd
/	octal;		'o
/	hex;		'x
/	float;		'f
/	scien;		'e
	charac;		'c
	string;		's
	longorunsg;	'l
/	unsigned;	'u
/	remote;		'r
	0;  0
	.text

decimal:
	mov	(r4)+,r1
	bge	1f
	neg	r1
	movb	$'-,(r3)+
	br	1f

longorunsg:
	cmpb	*formp,$'d
	bne	unsigned
	inc	formp
	br	long
	
unsigned:
	mov	(r4)+,r1
1:
	jsr	pc,1f
	br	prbuf
1:
	clr	r0
	div	$10.,r0
	mov	r1,-(sp)
	mov	r0,r1
	beq	1f
	jsr	pc,1b
1:
	mov	(sp)+,r0
	add	$'0,r0
	movb	r0,(r3)+
	rts	pc

long:
	mov	(r4)+,r2
	mov	(r4)+,r0
	mov	r4,-(sp)
	mov	r3,r4
	mov	r0,r3
	tst	r2
	bpl	1f
	neg	r2
	neg	r3
	sbc	r2
	movb	$'-,(r4)+
1:
	jsr	pc,1f
	mov	r4,r3
	mov	(sp)+,r4
	br	prbuf

1:
	clr	r1
	mov	$32.,r0
3:
	ashc	$1,r2
	rol	r1
	cmp	r1,$10.
	blo	2f
	sub	$10.,r1
	inc	r3
2:
	sob	r0,3b
	add	$'0,r1
	mov	r1,-(sp)
	ashc	$0,r2
	beq	1f
	jsr	pc,1b
1:
	movb	(sp)+,(r4)+
	rts	pc


charac:
	movb	(r4)+,(r3)+
	bne	1f
	dec	r3
1:
	movb	(r4)+,(r3)+
	bne	prbuf
	dec	r3
	br	prbuf

string:
	mov	ndigit,r1
	clr	r3
	mov	(r4),r2
1:
	tstb	(r2)+
	beq	1f
	inc	r3
	sob	r1,1b
1:
	mov	(r4)+,r2
	br	prstr

/hex:
/	mov	$1f,r2
/	.data
/1:
/	-4; !17
/	.text
/	br	2f
/
/octal:
/	mov	$1f,r2
/	.data
/1:
/	-3; !7
/	.text
/2:
/	mov	(r4)+,r1
/	beq	2f
/	tst	ndigit
/	beq	2f
/	movb	$'0,(r3)+
/2:
/	clr	r0
/	jsr	pc,1f
/	br	prbuf
/1:
/	mov	r1,-(sp)
/	ashc	(r2),r0
/	beq	1f
/	jsr	pc,1b
/1:
/	mov	(sp)+,r0
/	bic	2(r2),r0
/	add	$'0,r0
/	cmp	r0,$'9
/	ble	1f
/	add	$'A-'0-10.,r0
/1:
/	movb	r0,(r3)+
/	rts	pc
/
/float:
/	mov	ndigit,r0
/	mov	ndfnd,r2
/	jsr	pc,pfloat
/	br	prbuf
/
/scien:
/	mov	ndigit,r0
/	mov	ndfnd,r2
/	jsr	pc,pscien
/	br	prbuf
/
/remote:
/	mov	(r4)+,r4
/	mov	(r4)+,formp
/	jmp	loop
/
prbuf:
	mov	sp,r2
	add	$4,r2
	sub	r2,r3
prstr:
	mov	r4,-(sp)
	mov	$' ,-(sp)
	mov	r3,r4
	neg	r3
	add	width,r3
	ble	1f
/	tst	rjust
/	bne	1f
2:
	jsr	pc,*$_putchar
	sob	r3,2b
1:
	tst	r4
	beq	2f
1:
	movb	(r2)+,(sp)
	jsr	pc,*$_putchar
	sob	r4,1b
2:
	tst	r3
	ble	1f
	mov	$' ,(sp)
2:
	jsr	pc,*$_putchar
	sob	r3,2b
1:
	tst	(sp)+
	mov	(sp)+,r4
	jmp	loop

gnum:
/	clr	ndfnd
	clr	r1
1:
	movb	*formp,r0
	inc	formp
	sub	$'0,r0
	cmp	r0,$'*-'0
	bne	2f
	mov	(r4)+,r0
	br	3f
2:
	cmp	r0,$9.
	bhi	1f
3:
/	inc	ndfnd
	mul	$10.,r1
	add	r0,r1
	br	1b
1:
	add	$'0,r0
	rts	r3

.bss
width:	.=.+2
formp:	.=.+2
/rjust:	.=.+2
/ndfnd:	.=.+2
ndigit:	.=.+2
