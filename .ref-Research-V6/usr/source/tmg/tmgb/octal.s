i = r3
.globl putcall,iget,kput,putoct
.globl generate,succ
.globl octal

octal:
	mov	$1f+1,r0
	jsr	pc,putcall
	jsr	pc,iget
	mov	(r0),r0
	jsr	pc,kput
	jmp	succ
1:
	mov	(i),r0
	jsr	pc,putoct
	jmp	generate
