.globl allocate,putword,putchar
.globl iget
.globl succ
.globl table

table:
	mov	$10,r0
	jsr	pc,allocate
	mov	r1,-(sp)
	clr	r0
	jsr	pc,putword
	jsr	pc,putword
	jsr	pc,putword
	jsr	pc,putchar
	jsr	pc,iget
	mov	(sp)+,(r0)
	jmp	succ
