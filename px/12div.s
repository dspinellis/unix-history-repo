/
/ INTEGER DIVISION
/
_DIV42:
	movif	(sp)+,fr0
	tst	(sp)
	sxt	-(sp)
	br	2f
_DIV24:
	mov	(sp)+,r2
	mov	(sp)+,r0
	mov	(sp)+,r1
	div	r2,r0
	bvs	1f
	mov	r0,-(sp)
	sxt	-(sp)
	return
1:
	sub	$6.,sp
	tst	r2
	sxt	-(sp)
_DIV4:
	movif	(sp)+,fr0
2:
	cfcc
	beq	9f
	movif	(sp)+,fr1
	divf	fr0,fr1
	movfi	fr1,-(sp)
	return
_DIV2:
	mov	(sp)+,r2
	beq	9f
	mov	(sp)+,r1
	sxt	r0
	div	r2,r0
	bvs	1f
	mov	r0,-(sp)
	sxt	-(sp)
	return
1:
	mov	$100000,-(sp)
	sxt	-(sp)
	return
9:
	mov	$EDIVCHK,_perrno
	error	EDIVCHK
