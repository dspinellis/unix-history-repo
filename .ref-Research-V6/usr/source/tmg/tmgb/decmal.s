i = r3
.globl putcall,iget,kput,putdec
.globl generate,succ
.globl decimal

decimal:
	mov	$1f+1,r0
	jsr	pc,putcall
	jsr	pc,iget
	mov	(r0),r0
	jsr	pc,kput
	jmp	succ
1:
	mov	(i),r0
	jsr	pc,putdec
	jmp	generate
