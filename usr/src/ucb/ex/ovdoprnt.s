/ C library -- conversions
/ Overlay modification -- wfj 8/80
/ stack frame is one word larger...
/ sccs id @(#)ovdoprnt.s	1.2 8/26/80
/

/ width=-8.
/ formp=-10.
/ rjust=-12.
/ ndfnd=-14.
/ ndigit=-16.
/ zfill=-18.
width=-10.
formp=-12.
rjust=-14.
ndfnd=-16.
ndigit=-18.
zfill=-20.
.globl	__doprnt

.globl	__strout
.globl	csv
.globl	cret

__doprnt:
	jsr	r5,csv
	sub	$128.+12.,sp
	mov	4(r5),formp(r5)		/ format
	mov	6(r5),r4
loop:
	mov	sp,r3
	mov	formp(r5),r1
2:
	movb	(r1)+,r2
	beq	2f
	cmp	r2,$'%
	beq	2f
	movb	r2,(r3)+
	br	2b
2:
	mov	r1,formp(r5)
	cmp	r3,sp
	beq	2f
	mov	sp,r0
	mov	8(r5),-(sp)
	clr	-(sp)
	mov	r3,-(sp)
	sub	r0,(sp)
	mov	r0,-(sp)
	jsr	pc,__strout
	add	$8,sp
2:
	tst	r2
	bne	2f
	jmp	cret
2:
	mov	sp,r3
2:
	clr	rjust(r5)
	clr	ndigit(r5)
	mov	$' ,zfill(r5)
	cmpb	*formp(r5),$'-
	bne	2f
	inc	formp(r5)
	inc	rjust(r5)
2:
	cmpb	*formp(r5),$'0
	bne	2f
	mov	$'0,zfill(r5)
2:
	jsr	r3,gnum
	mov	r1,width(r5)
	clr	ndfnd(r5)
	cmp	r0,$'.
	bne	1f
	jsr	r3,gnum
	mov	r1,ndigit(r5)
1:
	mov	$swtab,r1
1:
	mov	(r1)+,r2
	bne	2f
	movb	r0,(r3)+
	jmp	prbuf
2:
	cmp	r0,(r1)+
	bne	1b
	jmp	(r2)
	.data
swtab:
	decimal;	'd
	octal;		'o
	hex;		'x
	charac;		'c
	string;		's
	longorunsg;	'l
	longorunsg;	'L
	unsigned;	'u
	remote;		'r
	long;		'D
	loct;		'O
	lhex;		'X
	lunsigned;	'U
	0;  0
	.text

longorunsg:
	movb	*formp(r5),r0
	inc	formp(r5)
	cmp	r0,$'o
	beq	loct
	cmp	r0,$'x
	beq	lhex
	cmp	r0,$'d
	beq	long
	cmp	r0,$'u
	beq	lunsigned
	dec	formp(r5)
	br	unsigned

octal:
	clr	r0
	br	1f
loct:
	mov	(r4)+,r0
1:
	mov	$8.,r2
	br	2f

hex:
	clr	r0
	br	1f

lhex:
	mov	(r4)+,r0
1:
	mov	$16.,r2
2:
	mov	(r4)+,r1
	br	compute

decimal:
	mov	(r4)+,r1
	sxt	r0
	bmi	3f
	br	2f

unsigned:
	clr	r0
	br	1f

long:
	mov	(r4)+,r0
	bge	1f
	mov	(r4)+,r1
3:
	neg	r0
	neg	r1
	sbc	r0
	movb	$'-,(r3)+
	br	2f

lunsigned:
	mov	(r4)+,r0
1:
	mov	(r4)+,r1
2:
	mov	$10.,r2

/
/ Algorithm courtesy Keith Davis
/
compute:
	mov	r5,-(sp)
	mov	r4,-(sp)
	mov	r0,r4
	mov	ndigit(r5),r0
	mov	r1,r5
	ashc	$0,r4
	beq	1f
	tst	r0
	beq	1f
	movb	$'0,(r3)+
1:
	jsr	pc,1f
	mov	(sp)+,r4
	mov	(sp)+,r5
	br	prbuf

1:
	clr	r0
	mov	r4,r1
	beq	2f
	div	r2,r0
	mov	r0,r4
	mov	r1,r0
2:
	mov	r5,r1
	asl	r2
	div	r2,r0
	asr	r2
	asl	r0
	cmp	r2,r1
	bgt	2f
	sub	r2,r1
	inc	r0
2:
	mov	r1,-(sp)
	mov	r0,r5
	bne	2f
	tst	r4
	beq	1f
2:
	jsr	pc,1b
1:
	mov	(sp)+,r0
	add	$'0,r0
	cmp	r0,$'9
	ble	1f
	add	$'a-'0-10.,r0
1:
	movb	r0,(r3)+
	rts	pc
	
charac:
	mov	$' ,zfill(r5)
	mov	(r4)+,r0
	bic	$!377,r0
	beq	prbuf
	movb	r0,(r3)+
	br	prbuf

string:
	mov	$' ,zfill(r5)
	mov	ndigit(r5),r1
	mov	(r4),r2
	mov	r2,r3
	bne	1f
	mov	$nulstr,r2
	mov	r2,r3
	mov	r2,(r4)
1:
	tstb	(r2)+
	beq	1f
	inc	r3
	sob	r1,1b
1:
	mov	(r4)+,r2
	br	prstr

remote:
	mov	(r4)+,r4
	mov	(r4)+,formp(r5)
	jmp	loop

prbuf:
	mov	sp,r2
prstr:
	sub	r2,r3
	mov	width(r5),r1
	sub	r3,r1
	bge	1f
	clr	r1
1:
	tst	rjust(r5)
	bne	1f
	neg	r1
1:
	mov	zfill(r5),-(sp)
	mov	8(r5),-(sp)
	mov	r1,-(sp)
	mov	r3,-(sp)
	mov	r2,-(sp)
	jsr	pc,__strout
	add	$10.,sp
	jmp	loop

gnum:
	clr	ndfnd(r5)
	clr	r1
1:
	movb	*formp(r5),r0
	inc	formp(r5)
	sub	$'0,r0
	cmp	r0,$'*-'0
	bne	2f
	mov	(r4)+,r0
	br	3f
2:
	cmp	r0,$9.
	bhi	1f
3:
	inc	ndfnd(r5)
	mul	$10.,r1
	add	r0,r1
	br	1b
1:
	add	$'0,r0
	rts	r3

.data
nulstr:
	<(null)\0>
