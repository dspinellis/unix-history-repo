g = r4
.globl succ,iget
.globl trans

trans:
	jsr	pc,iget
	mov	r0,(g)+
	jmp	succ
