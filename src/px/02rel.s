/
/ RELATIONAL OPERATORS
/
_REL2:
	mov	(sp)+,r0
	cmp	(sp)+,r0
	jmp	*reltab(r3)
_REL42:
	mov	(sp)+,r0
	mov	(sp)+,r1
	tst	(sp)
	sxt	r2
	br	2f
_REL24:
	mov	(sp)+,r1
	sxt	r0
	br	1f
_REL4:
	mov	(sp)+,r0
	mov	(sp)+,r1
1:
	mov	(sp)+,r2
2:
	cmp	r2,r0
	bne	1f
	cmp	(sp)+,r1
	bhi	3f
	beq	2f
	tst	$-1
2:
	jmp	*reltab(r3)
3:
	tst	$1
	jmp	*reltab(r3)
1:
	tst	(sp)+
	cmp	r2,r0
	jmp	*reltab(r3)
_REL28:
	tst	(sp)
	sxt	-(sp)
_REL48:
	movif	(sp)+,fr0
	cmpf	(sp)+,fr0
	cfcc
	jmp	*reltab(r3)
_REL82:
	movf	(sp)+,fr0
	tst	(sp)
	sxt	-(sp)
	br	1f
_REL84:
	movf	(sp)+,fr0
1:
	movif	(sp)+,fr2
	cmpf	fr2,fr0
	cfcc
	jmp	*reltab(r3)
_REL8:
	movf	(sp)+,fr0
	cmpf	(sp)+,fr0
	cfcc
	jmp	*reltab(r3)
_RELG:
	mov	sp,r0
	mov	(lc),r2
	inc	r2
	bic	$1,r2
	add	r2,sp
	mov	sp,r1
	add	r2,sp
	mov	(lc)+,r2
1:
	cmpb	(r1)+,(r0)+
	bne	1f
	sob	r2,1b
	tst	$0
1:
	jmp	*reltab(r3)
.data
reltab:
	releq
	relne
	rellt
	relgt
	relle
	relge
	ifeq
	ifne
	iflt
	ifgt
	ifle
	ifge
.text
releq:
	beq	true
	clr	-(sp)
	return
relne:
	bne	true
	clr	-(sp)
	return
rellt:
	blt	true
	clr	-(sp)
	return
relgt:
	bgt	true
	clr	-(sp)
	return
relle:
	ble	true
	clr	-(sp)
	return
relge:
	bge	true
	clr	-(sp)
	return
true:
	mov	$1,-(sp)
	return
false:
	clr	-(sp)
	return
ifeq:
	bne	iftra
	tst	(lc)+
	return
ifne:
	beq	iftra
	tst	(lc)+
	return
iflt:
	bge	iftra
	tst	(lc)+
	return
ifgt:
	ble	iftra
	tst	(lc)+
	return
ifle:
	bgt	iftra
	tst	(lc)+
	return
ifge:
	blt	iftra
	tst	(lc)+
	return
iftra:
	add	(lc),lc
	return
