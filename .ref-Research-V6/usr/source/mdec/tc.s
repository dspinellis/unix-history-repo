tcdt = 177350
tccm = 177342
tread:
	mov	$tcdt,r0
	mov	$tccm,r1
for:
	mov	$3,(r1)			/ rbn for
1:
	tstb	(r1)
	bge	1b
	tst	(r1)
	blt	rev
	cmp	tapa,(r0)
	beq	rd
	bgt	for

rev:
	mov	$4003,(r1)		/ rbn bac
1:
	tstb	(r1)
	bge	1b
	tst	(r1)
	blt	for
	mov	(r0),r2
	add	$5,r2
	cmp	tapa,r2
	blt	rev
	br	for

rd:
	mov	ba,-(r0)			/ bus addr
	mov	wc,-(r0)			/ wc
	mov	$5,-(r0)			/ read
1:
	tstb	(r1)
	bge	1b
	tst	(r1)
	blt	tread
	rts	pc

rew:
	mov	$4003,tccm
	rts	pc
