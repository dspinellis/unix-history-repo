/ C library -- unprintf


.globl	_unprintf
.globl	ndigix

.globl	pfloat
.globl	pscien
.globl	_IEH3revp
.globl	_IEH3bcko

_unprintf:
	mov	r5,-(sp)
	mov	sp,r5
	mov	4(r5),formp		/ format
	mov	r5,r4
	add	$6,r4			/ arglist
	sub	$128.,sp		/ buffer
loop:
	movb	*formp,r0
	beq	1f
	inc	formp
	cmp	r0,$'%
	beq	2f
3:
	mov	r4,-(sp)
	mov	r0,-(sp)
	jsr	pc,*$_IEH3revp
	tst	(sp)+
	mov	(sp)+,r4
	br	loop
1:
	add	$128.,sp
	mov	(sp)+,r5
	jsr	pc,_IEH3bcko
	rts	pc
2:
	clr	rjust
	clr	ndigix
	cmpb	*formp,$'-
	bne	2f
	inc	formp
	inc	rjust
2:
	jsr	r3,gnum; width
	clr	ndfnd
	cmp	r0,$'.
	bne	1f
	jsr	r3,gnum; ndigix
1:
	mov	sp,r3
	mov	$swtab,r1
1:
	mov	(r1)+,r2
	beq	3b
	cmp	r0,(r1)+
	bne	1b
	jmp	(r2)
swtab:
	decimal;	'd
	octal;		'o
	float;		'f
	scien;		'e
	charac;		'c
	string;		's
	logical;	'l
	0;  0

decimal:
	mov	(r4)+,r1
	bge	1f
	neg	r1
	movb	$'-,(r3)+
	br	1f

logical:
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

charac:
	movb	(r4)+,(r3)+
	bne	1f
	dec	r3
1:
	movb	(r4)+,(r3)+
	br	prbuf

string:
	mov	ndigix,r1
1:
	mov	(r4)+,r2
1:
	movb	(r2)+,(r3)+
	beq	prbuf
	sob	r1,1b
	br	prbuf

octal:
	mov	(r4)+,r1
	jsr	pc,1f
	br	prbuf
1:
	mov	r1,-(sp)
	beq	1f
	ash	$-3,r1
	bic	$!17777,r1
	jsr	pc,1b
1:
	mov	(sp)+,r0
	bic	$!7,r0
	add	$'0,r0
	movb	r0,(r3)+
	rts	pc

float:
	mov	ndfnd,r2
	jsr	pc,pfloat
	br	prbuf

scien:
	mov	ndfnd,r2
	jsr	pc,pscien
	br	prbuf

prbuf:
	cmp	r3,sp
	blos	1f
	tstb	-1(r3)
	bne	1f
	dec	r3
	br	prbuf
1:
	mov	sp,r2
	mov	r4,-(sp)
	mov	$' ,-(sp)
	mov	width,r1
	sub	r3,r1
	clrb	(r3)+
	add	r2,r1
	mov	r1,spaces
	ble	1f
	tst	rjust
	bne	1f
2:
	jsr	pc,*$_IEH3revp
	dec	spaces
	bne	2b
1:
	mov	r2,bufp
1:
	movb	*bufp,(sp)
	beq	1f
	inc	bufp
	jsr	pc,*$_IEH3revp
	br	1b
1:
	tst	spaces
	ble	1f
	tst	rjust
	beq	1f
	mov	$' ,(sp)
2:
	jsr	pc,*$_IEH3revp
	dec	spaces
	bne	2b
1:
	tst	(sp)+
	mov	(sp)+,r4
	jmp	loop

gnum:
	clr	ndfnd
	clr	r1
1:
	movb	*formp,r0
	inc	formp
	sub	$'0,r0
	cmp	r0,$9.
	bhi	1f
	inc	ndfnd
	mul	$10.,r1
	add	r0,r1
	br	1b
1:
	add	$'0,r0
	mov	r1,*(r3)+
	rts	r3

	.bss
ndigix:	.=.+2
width:	.=.+2
formp:	.=.+2
rjust:	.=.+2
ndfnd:	.=.+2
bufp:	.=.+2
spaces:	.=.+2
