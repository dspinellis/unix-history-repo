/
/ SET OPERATIONS
/
_ADDT:
	bne	1f
	mov	(lc)+,r3
1:
	inc	r3
	bic	$1,r3
	mov	sp,r0
	add	r3,sp
	mov	sp,r1
	asr	r3
1:
	bis	(r0)+,(r1)+
	sob	r3,1b
	return
_SUBT:
	bne	1f
	mov	(lc)+,r3
1:
	inc	r3
	bic	$1,r3
	mov	sp,r0
	add	r3,sp
	mov	sp,r1
	asr	r3
1:
	bic	(r0)+,(r1)+
	sob	r3,1b
	bic	(lc)+,-(r1)
	return
_MULT:
	bne	1f
	mov	(lc)+,r3
1:
	inc	r3
	bic	$1,r3
	mov	sp,r0
	add	r3,sp
	mov	sp,r1
	asr	r3
1:
	mov	(r0)+,r2
	com	r2
	bic	r2,(r1)+
	sob	r3,1b
	return
_CARD:
	bne	1f
	mov	(lc)+,r3
1:
	mov	r3,r1
	mov	sp,r2
	inc	r3
	bic	$1,r3
	add	r3,sp
	clr	r3
1:
	movb	(r2),r0
	bic	$!17,r0
	asl	r0
	add	poptab(r0),r3
	movb	(r2)+,r0
	ash	$-3,r0
	bic	$!36,r0
	add	poptab(r0),r3
	sob	r1,1b
	mov	r3,-(sp)
	return
.data
poptab:
	0	/ 0000
	1	/ 0001
	1	/ 0010
	2	/ 0011
	1	/ 0100
	2	/ 0101
	2	/ 0110
	3	/ 0111
	1	/ 1000
	2	/ 1001
	2	/ 1010
	3	/ 1011
	2	/ 1100
	3	/ 1101
	3	/ 1110
	4	/ 1111
.text
_CTTOT:
	bne	1f
	mov	(lc)+,r3
1:
	mov	r3,-(sp)
	mov	(lc)+,-(sp)
	mov	(lc)+,-(sp)
	jsr	pc,_pcttot
	mov	r0,sp
	return
_IN:
	bne	1f
	mov	(lc)+,r3
1:
	mov	(sp)+,r2
	sub	(lc)+,r2
	bmi	8f
	cmp	r2,(lc)+
	bgt	9f
	mov	r2,r1
	bic	$!7,r1
	ash	$-3,r2
	add	sp,r2
	inc	r3
	bic	$1,r3
	add	r3,sp
	mov	$1,r0
	ash	r1,r0
	bitb	r0,(r2)
	jne	true
	jmp	false
8:
	tst	(lc)+
9:
	inc	r3
	bic	$1,r3
	add	r3,sp
	jmp	false
_INCT:
	mov	(sp)+,r2
	mov	(sp)+,r3
1:
	cmp	r2,(sp)+
	blt	2f
	cmp	r2,(sp)+
	bgt	3f
	dec	r3
	add	r3,r3
	add	r3,r3
	add	r3,sp
	mov	$1,-(sp)
	return
2:
	tst	(sp)+
3:
	sob	r3,1b
	clr	-(sp)
	return
