/ rp03 disk driver

rpda = 176724
	mov	dska,r1
	clr	r0
	div	$10.,r0
	mov	r1,-(sp)
	mov	r0,r1
	clr	r0
	div	$20.,r0
	bisb	r1,1(sp)
	mov	$rpda,r1
	mov	(sp)+,(r1)
	mov	r0,-(r1)
	mov	ba,-(r1)
	mov	wc,-(r1)
	mov	$iocom,-(r1)
1:
	tstb	(r1)
	bpl	1b
	rts	pc
