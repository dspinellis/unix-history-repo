.globl succ
.globl .ng,.nt,.cm

/ !

.nt:
	tst	(sp)
	beq	1f
	clr	(sp)
	br	9f
1:
	mov	$1,(sp)
	br	9f

/unary -

.ng:
	neg	(sp)
	br	9f

/ ~

.cm:
	com	(sp)

9:
	jmp	succ
