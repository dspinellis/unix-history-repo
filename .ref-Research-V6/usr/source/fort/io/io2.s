/
/

/ io2 -- format cracker

.globl	iowf
.globl	iowp
.globl	iorf

iowp:
	mov	(sp)+,formp
	mov	$6,r1
	br	1f

iowf:
	mov	(sp)+,formp
	tst	(sp)+
	mov	(sp)+,r1
1:
	jsr	r5,setio; 2		/ write
	clr	rdflg
	br	1f

iorf:
	mov	(sp)+,formp
	tst	(sp)+
	mov	(sp)+,r1
	jsr	r5,setio; 1		/ read
	mov	pc,rdflg

1:
	clr	binflg
	clr	slcnt
	clr	itmfnd
	clr	scale
	clr	itmflg
	mov	$pbuf,ppar
	mov	$-1,llpcnt
	jsr	r5,fmtchr
	mov	formp,llp
	cmp	r0,$'(
	beq	crack
	jsr	r5,rerr; 106.
	sys	exit
crack:
	clr	ngflg
	mov	$1,rep
item:
	jsr	r5,fmtchr
	cmp	$' ,r0
	beq	item
	cmp	$'\t,r0
	beq	item
	jsr	r5,switch; mswitch
	jsr	r5,rerr; 105.
	sys	exit

mswitch:
	'a; afmt
	'f; ffmt
	'e; efmt
	'g; gfmt
	'd; dfmt
	'i; ifmt
	'l; lfmt
	'h; hfmt
	'x; xfmt
	'p; scal
	'-; minus
	'0; numb
	'1; numb
	'2; numb
	'3; numb
	'4; numb
	'5; numb
	'6; numb
	'7; numb
	'8; numb
	'9; numb
	',; crack
	'/; slash
	'(; lpar
	'); rpar
	'"; quote
	' ; item
	0; 0

minus:
	jsr	r5,gnum
	neg	r0
	br	1f
numb:
	dec	formp
	jsr	r5,gnum
1:
	mov	r0,rep
	br	item

scal:
	mov	rep,scale
	br	crack

elist:
	tst	_nocr
	beq	1f
	tst	rdflg
	bne	1f
	jsr	r5,eorec1
	br	2f
1:
	jsr	r5,eorec
2:
	jmp	*(r4)+

slash:
	jsr	r5,eorec
	br	crack

rpar:
	mov	ppar,r0
	cmp	r0,$pbuf		/ see if outer parens
	blos	2f
	dec	-2(r0)
	ble	1f		/ no repeats left
	mov	-4(r0),formp	/ reset scan
	br	crack
1:
	sub	$4,ppar
	br	crack		/ pop parens
2:
	jsr	r5,getitm
		br elist
	tst	itmfnd
	bne	1f
	jsr	r5,rerr; 107.
	sys	exit
1:
	jsr	r5,eorec
	inc	itmflg
	mov	llpcnt,r1
	bpl	1f
	mov	llp,formp
	jmp	crack
1:
	mov	llp,r2
	mov	r2,formp
	mov	ppar,r0
	mov	r2,(r0)+
	mov	r1,(r0)+
	mov	r0,ppar
	jbr	crack1

lpar:
	mov	ppar,r0
	cmp	r0,$pbuf+10
	blo	1f
	jsr	r5,rerr; 108.
	sys	exit
1:
	mov	formp,(r0)+
	mov	rep,(r0)+
	mov	r0,ppar
	cmp	r0,$pbuf+4
	bhi	1f
	mov	formp,llp
	mov	rep,llpcnt
1:
	jmp	crack
quote:
	inc	ngflg
	mov	$44,-(sp)
	br	3f

hfmt:
	inc	ngflg
	mov	$40,-(sp)
	br	3f

xfmt:
	inc	ngflg
	mov	$34,-(sp)
	br	3f

afmt:
	mov	$30,-(sp)
	br	1f

ifmt:
	clr	-(sp)
	br	1f

lfmt:
	mov	$4,-(sp)
1:
	jsr	r5,gnum
	mov	r0,width
	br	2f

ffmt:
	mov	$10,-(sp)
	br	1f

dfmt:
	mov	$14,-(sp)
	br	1f

gfmt:
	mov	$20,-(sp)
	br	1f

efmt:
	mov	$24,-(sp)

1:
	jsr	r5,gnum
	mov	r0,width
4:
	jsr	r5,fmtchr
	cmp	r0,$' /
	beq	4b
	cmp	r0,$'.
	bne	err1
	jsr	r5,gnum
	mov	r0,ndig
2:
	inc	itmfnd
3:
	add	$cvsw,(sp)
	tst	rdflg
	beq	1f
	add	$2,(sp)
1:
	mov	*(sp)+,-(sp)
1:
	tst	ngflg
	bne	2f
	jsr	r5,getitm
		br 1f
2:
	clr	gflg
	jsr	r5,*(sp)
	dec	rep
	bgt	1b
	tst	(sp)+
	br	crack1
1:
	tst	(sp)+
	jmp	elist

cvsw:
	iocv; iicv	/ 0
	locv; licv	/ 4
	focv; ficv	/ 10
	docv; dicv	/ 14
	gocv; gicv	/ 20
	eocv; eicv	/ 24
	aocv; aicv	/ 30
	xocv; xicv	/ 34
	hocv; hicv	/ 40
	qocv; qicv	/ 44

crack1:
	jmp	crack

err1:
	jsr	r5,rerr; 109.
	sys	exit

