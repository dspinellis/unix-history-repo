f = r5
g = r4
.globl g1
.globl pbundle,iget
.globl succ
.globl bundle,reduce

reduce:
	jsr	pc,iget
	mov	(r0),r1
	mov	g,r0
	asl	r1
	sub	r1,r0
	br	1f
	br	1f
bundle:
	mov	f,r0
	add	$g1,r0
1:
	jsr	pc,pbundle
	tst	r0
	beq	1f
	mov	r0,(g)+
1:
	jmp	succ
