/
/ CASE OPERATORS
/
_CASE1OP:
	bne	1f
	mov	(lc)+,r3
1:
	mov	lc,r0
	add	r3,r0
	add	r3,r0
	mov	r3,r2
	tst	(sp)+
1:
	cmpb	(r0)+,-2(sp)
	beq	5f
	sob	r3,1b
	mov	$ECASE,_perrno
	error	ECASE
_CASE2OP:
	bne	1f
	mov	(lc)+,r3
1:
	mov	lc,r0
	add	r3,r0
	add	r3,r0
	mov	r3,r2
	tst	(sp)+
1:
	cmp	(r0)+,-2(sp)
	beq	5f
	sob	r3,1b
	mov	$ECASE,_perrno
	error	ECASE
_CASE4OP:
	bne	1f
	mov	(lc)+,r3
1:
	mov	lc,r0
	add	r3,r0
	add	r3,r0
	mov	r3,r2
	cmp	(sp)+,(sp)+
1:
	cmp	(r0)+,-4(sp)
	beq	2f
	tst	(r0)+
	br	3f
2:
	cmp	(r0)+,-2(sp)
	beq	5f
3:
	sob	r3,1b
	mov	$ECASE,_perrno
	error	ECASE
5:
	sub	r3,r2
	add	r2,r2
	add	lc,r2
	add	(r2),lc
	return
