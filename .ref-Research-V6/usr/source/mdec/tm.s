mts = 172520
mtc = 172522
mtbrc = 172524
mtcma = 172526

tread:
1:
	mov	ba,mtma
	cmp	mtapa,tapa
	beq	1f
	bhi	2f
	jsr	pc,rrec
	br	1b
2:
	jsr	pc,rew
	br	1b
1:
	mov	wc,r1
1:
	jsr	pc,rrec
	add	$256.,r1
	bmi	1b
	rts	pc

rrec:
	mov	$mts,r0
	bit	$2,(r0)+		/ rewind status
	bne	rrec
	tstb	(r0)+		/ cu ready
	bpl	rrec
	inc 	r0
	mov	$-512.,(r0)+	/ byte count
	mov	mtma,(r0)	/ bus address
	mov	$mtc,r0
	mov	$60003,(r0)		/ read 800bpi
1:
	tstb	(r0)
	bpl	1b
	tst	(r0)+
	bpl	1f
	mov	$-1,(r0)
	mov	$60013,-(r0)		/ backspace
	br	rrec
1:
	add	$512.,mtma
	inc	mtapa
	rts	pc

rew:
	mov	$60017,*$mtc
	clr	mtapa
	rts	pc

mtapa:	0
mtma:	0
