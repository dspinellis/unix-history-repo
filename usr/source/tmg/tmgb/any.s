i = r3
f = r5
.globl x,j
.globl succ,fail
.globl	ctest,iget
.globl any

any:
	mov	j(f),-(sp)
	jsr	pc,iget
	jsr	pc,ctest
	bcs	1f
	mov	(sp)+,j(f)
	jmp	fail
1:
	tst	(sp)+
	jmp	succ
