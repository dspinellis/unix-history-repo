f = r5
.globl n
.globl	succ,iget
.globl ignore

ignore:
	jsr	pc,iget
	mov	(r0),n(f)
	jmp	succ
