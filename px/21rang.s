/
/ range checking
/
_RANG42:
	bne	1f
	mov	(lc)+,r3
1:
	mov	2(sp),r0
	sxt	r1
	cmp	r1,(sp)
	bne	9f
	br	2f
_RANG2:
	bne	1f
	mov	(lc)+,r3
1:
	mov	(sp),r0
2:
	cmp	r0,r3
	blt	9f
	cmp	r0,(lc)+
	bgt	9f
	return
_RANGS42:
	bne	1f
	mov	(lc)+,r3
1:
	mov	2(sp),r0
	sxt	r1
	cmp	r1,(sp)
	bne	9f
	cmp	r0,r3
	bhi	9f
	return
_RANGS2:
	bne	1f
	mov	(lc)+,r3
1:
	cmp	(sp),r3
	bhi	9f
	return
_RANGS4:
	mov	(sp),r0
	bmi	9f
	mov	2(sp),r1
	br	2f
_RANGS24:
	mov	(sp),r1
	bmi	9f
	sxt	r0
	br	2f
_RANG24:
	mov	(sp),r1
	sxt	r0
	br	1f
_RANG4:
	mov	(sp),r0
	mov	2(sp),r1
1:
	cmp	r0,(lc)+
	blt	9f
	bgt	1f
	cmp	r1,(lc)
	blo	9f
1:
	tst	(lc)+
2:
	cmp	r0,(lc)+
	bgt	9f
	blt	1f
	cmp	r1,(lc)
	bhi	9f
1:
	tst	(lc)+
	return
9:
	mov	$ERANGE,_perrno
	error	ERANGE
