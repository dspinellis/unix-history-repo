/
/ ASSIGNMENT OPERATORS
/
_AS42:
	tst	(sp)+
_AS2:
	mov	(sp)+,*(sp)+
	return
_AS4:
	mov	(sp)+,r0
	mov	(sp)+,r1
1:
	mov	(sp)+,r2
	mov	r0,(r2)+
	mov	r1,(r2)
	return
_AS24:
	mov	(sp)+,r1
	sxt	r0
	br	1b
_AS41:
	tst	(sp)+
_AS21:
	movb	(sp)+,*(sp)+
	return
_AS28:
	tst	(sp)
	sxt	-(sp)
_AS48:
	movif	(sp)+,fr0
	movf	fr0,*(sp)+
	return
_AS8:
	mov	$8.,r3
_AS:

	bne	3f
	mov	(lc)+,r3
3:
	mov	sp,r2
	add	r3,r2
	inc	r2
	bic	$1,r2
	mov	(r2),r2
	clc
	ror	r3
	beq	2f
1:
	mov	(sp)+,(r2)+
	sob	r3,1b
2:
	bcc	1f
	movb	(sp)+,(r2)+
1:
	tst	(sp)+
	return
