/ C library -- nargs

.globl	_nargs

_nargs:
	mov	2(r5),r1		/ pc of caller of caller
	mov	r5,-(sp)
	mov	sp,r5
	clr	r0
	cmp	-4(r1),jsrsd
	bne	1f
	mov	$2,r0
1:
	cmp	(r1),tsti
	bne	1f
	add	$2,r0
	br	2f
1:
	cmp	(r1),cmpi
	bne	1f
	add	$4,r0
	br	2f
1:
	cmp	(r1),addi
	bne	2f
	add	2(r1),r0
2:
	asr	r0
	mov	(sp)+,r5
	rts	pc

jsrsd:	jsr	pc,*$0
tsti:	tst	(sp)+
cmpi:	cmp	(sp)+,(sp)+
addi:	add	$0,sp

