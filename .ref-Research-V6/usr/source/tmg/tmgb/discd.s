.globl release
.globl discard
.globl iget,succ

discard:
	jsr	pc,iget
	mov	(r0),r1
	jsr	pc,release
	jmp	succ
