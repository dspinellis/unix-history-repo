/ default comparison routine for qsort
/ set codes for 
/
/	cmp	(r0),(r4)
/

.globl	compare

compare:
	mov	r3,-(sp)
	mov	r4,-(sp)
1:
	cmpb	(r0)+,(r4)+
	bne	1f
	dec	r3
	bne	1b
	clr	r0
	br	2f
1:
	blo	1f
	mov	$1,r0
	br	2f
1:
	mov	$-1,r0
2:
	mov	(sp)+,r4
	mov	(sp)+,r3
	tst	r0
	rts	pc

