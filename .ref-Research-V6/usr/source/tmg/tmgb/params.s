f = r5
.globl iget
.globl env,si
.globl succ
.globl params

params:
	jsr	pc,iget
	mov	(r0),r0
	asl	r0
	mov	env(f),r1
	add	r0,si(r1)
	jmp	succ
