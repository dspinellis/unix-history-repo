/
/ MODULO
/
_MOD42:
	movif	(sp)+,fr0
	tst	(sp)
	sxt	-(sp)
	br	2f
_MOD24:
	mov	(sp)+,r2
	mov	(sp)+,r0
	mov	(sp)+,r1
	div	r2,r0
	bvs	1f
	mov	r1,-(sp)
	sxt	-(sp)
	return
1:
	sub	$6.,sp
	tst	r2
	sxt	-(sp)
_MOD4:
	movif	(sp)+,fr0
2:
	cfcc
	beq	9f
	movif	(sp)+,fr1
	divf	fr0,fr1
	modf	$ONE,fr1
	mulf	fr0,fr1
	cfcc
	bgt	1f
	subf	$HALF,fr1
	br	2f
1:
	addf	$HALF,fr1
2:
	movfi	fr1,-(sp)
	return
_MOD2:
	mov	(sp)+,r2
	beq	9f
	mov	(sp)+,r1
	sxt	r0
	div	r2,r0
	bvs	1f
	mov	r1,-(sp)
	sxt	-(sp)
	return
1:
	clr	-(sp)
	sxt	-(sp)
	return
9:
	mov	$EMODCHK,_perrno
	error	EMODCHK
