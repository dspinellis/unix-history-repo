/ count subroutine calls during profiling

.globl	mcount
.comm	countbase,2

mcount:
	mov	(r0),r1
	bne	1f
	mov	countbase,r1
	beq	2f
	add	$6,countbase
	mov	(sp),(r1)+
	mov	r1,(r0)
1:
	inc	2(r1)
	bne	2f
	inc	(r1)
2:
	rts	pc

