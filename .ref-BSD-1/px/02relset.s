/
/ RELATIONALS ON SETS
/
_RELT:
	mov	(lc)+,r2
	inc	r2
	bic	$1,r2
	mov	sp,r1
	add	r2,sp
	mov	sp,r0
	add	r2,sp
	asr	r2
	cmp	r3,$10.
	ble	1f
	sub	$12.,r3
	mov	$2f,lp
1:
	jmp	*settab(r3)
2:
	mov	$loop,lp
	tst	(sp)+
	beq	1f
	tst	(lc)+
	return
1:
	add	(lc),lc
	return
.data
settab:
	seteq
	setne
	setlt
	setgt
	setle
	setge
.text
seteq:
	cmp	(r0)+,(r1)+
	bne	false
	sob	r2,seteq
	br	true
setne:
	cmp	(r0)+,(r1)+
	bne	true
	sob	r2,setne
	br	false
setlt:
	mov	(r0),r3
	bic	(r1),r3
	bne	false
	bic	(r0)+,(r1)+
	bne	4f		/ continue just <=
	sob	r2,setlt
	br	false
setgt:
	mov	(r1),r3
	bic	(r0),r3
	bne	false
	bic	(r1)+,(r0)+
	bne	5f		/ continue just >=
	sob	r2,setgt
	br	false
setle:
	bic	(r1)+,(r0)+
	bne	false
4:
	sob	r2,setle
	br	true
setge:
	bic	(r0)+,(r1)+
	bne	false
5:
	sob	r2,setge
	br	true
