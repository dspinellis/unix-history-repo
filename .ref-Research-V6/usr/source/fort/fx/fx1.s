/
/

/ fx1 -- utility

.globl	lookup
/ lookup
/ lookup string pointed at by r2
/ in table pointed at by arg1
/ r0 returns symbol number*2
/ r2 is advanced by matched string
/ registers used: r0,r2
lookup:
	mov	r1,-(sp)
	clr	r0
	mov	r2,-(sp)
	mov	(r5)+,r1
1:
	mov	(sp),r2
	tstb	(r1)
	beq	1f
2:
	cmpb	(r2)+,(r1)+
	bne	2f
	tstb	(r1)
	bne	2b
	asl	r0
	cmp	(r5)+,(sp)+
	mov	(sp)+,r1
	rts	r5
2:
	tstb	(r1)+
	bne	2b
	inc	r0
	br	1b
1:
	mov	(sp)+,r2
	mov	(sp)+,r1
	rts	r5

