/
/ FLOATING DIVISION
/
_DVD2:
	tst	(sp)
	sxt	-(sp)
_DVD42:
	movif	(sp)+,fr0
	br	1f
_DVD82:
	movf	(sp)+,fr0
1:
	cfcc
	beq	9f
	tst	(sp)
	sxt	-(sp)
	movif	(sp)+,fr2
	divf	fr0,fr2
	movf	fr2,-(sp)
	return
_DVD24:
	tst	(sp)
	sxt	-(sp)
_DVD4:
	movif	(sp)+,fr0
	br	1f
_DVD84:
	movf	(sp)+,fr0
1:
	cfcc
	beq	9f
	movif	(sp)+,fr2
	divf	fr0,fr2
	movf	fr2,-(sp)
	return
_DVD28:
	tst	(sp)
	sxt	-(sp)
_DVD48:
	movif	(sp)+,fr0
	br	1f
_DVD8:
	movf	(sp)+,fr0
1:
	cfcc
	beq	9f
	movf	(sp)+,fr2
	divf	fr0,fr2
	movf	fr2,-(sp)
	return
9:
	mov	$EFDIVCHK,_perrno
	error	EFDIVCHK
