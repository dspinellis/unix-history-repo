.globl .f
.globl succ
.globl seekchar,getword

.f:
	mov	(sp),r0
	mov	r0,6(sp)
	mov	4(sp),r1
	jsr	pc,seekchar
	jsr	pc,getword
	mov	r0,(sp)
	mov	$-1,2(sp)
	jmp	succ
