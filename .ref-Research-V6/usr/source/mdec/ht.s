htcs1 = 172440
htba  = 172444
htfc  = 172446
htcs2 = 172450
htds  = 172452
httc  = 172472

P800 = 1300
P1600 = 2300
PIP = 20000
MOL = 10000
ERR = 40000
REV = 33
READ = 71
REW = 7

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
	mov	$htds,r0
	tstb	(r0)
	bpl	rrec
	bit	$PIP,(r0)
	bne	rrec
	bit	$MOL,(r0)
	beq	rrec
	mov	$htfc,r0
	mov	$-512.,(r0)
	mov	mtma,-(r0)
	mov	$-256.,-(r0)
	mov	$READ,-(r0)
1:
	tstb	(r0)
	bpl	1b
	bit	$ERR,*$htds
	bpl	1f
	mov	$-1,*$htfc
	mov	$REV,(r0)
	br	rrec
1:
	add	$512.,mtma
	inc	mtapa
	rts	pc

rew:
	clr	*$htcs2
	mov	$P800,*$httc
	mov	$REW,*$htcs1
	clr	mtapa
	rts	pc

mtapa:	0
mtma:	0
