f = r5
i = r3
.globl j
.globl succ
.globl ctest,iget
.globl string

string:
	tst	-(sp)
	jsr	pc,iget
1:
	mov	j(f),(sp)
	mov	r0,-(sp)
	jsr	pc,ctest
	mov	(sp)+,r0
	bcs	1b
	mov	(sp)+,j(f)
	jmp	succ
