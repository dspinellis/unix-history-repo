/ format RK03/05 disk packs

rkda = 177412

	0
	5
	mov	$203.*2,r4
	clr	r3
1:
	mov	$rkda+2,r0
	mov	r3,-(r0)
	mov	$buf,-(r0)
	mov	$-12.*256.,-(r0)
	mov	$6003,-(r0)
2:
	tstb	(r0)
	bge	2b
	tst	(r0)
	blt	1f
	add	$20,r3
	dec	r4
	bne	1b
	rts	pc
1:
	jsr	pc,4(r5)
		<rkf: error\n\0>; .even
	rts	pc

buf:	.=.+2
