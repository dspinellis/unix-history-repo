/ C library -- long output conversion

.globl	_locv

_locv:
	mov	r5,-(sp)
	mov	sp,r5
	mov	r4,-(sp)
	mov	r3,-(sp)
	mov	r2,-(sp)
	mov	$buf,r4
	mov	6(r5),r3
	mov	4(r5),r2
	bpl	1f
	neg	r2
	neg	r3
	sbc	r2
	movb	$'-,(r4)+
1:
	jsr	pc,1f
	clrb	(r4)+
	mov	$buf,r0
	mov	(sp)+,r2
	mov	(sp)+,r3
	mov	(sp)+,r4
	mov	(sp)+,r5
	rts	pc

1:
	jsr	pc,divid
	add	$'0,r1
	mov	r1,-(sp)
	ashc	$0,r2
	beq	1f
	jsr	pc,1b
1:
	movb	(sp)+,(r4)+
	rts	pc

divid:
	clr	r1
	mov	$32.,r0
1:
	ashc	$1,r2
	rol	r1
	cmp	r1,$10.
	blo	2f
	sub	$10.,r1
	inc	r3
2:
	sob	r0,1b
	rts	pc


.bss
buf:	.=.+12.
