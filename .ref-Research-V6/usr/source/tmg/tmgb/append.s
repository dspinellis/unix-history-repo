.globl succ
.globl putcstr,iget
.globl append

append:
	jsr	pc,iget
	mov	r0,-(sp)
1:
	movb	*(sp),r0
	beq	1f
	jsr	pc,putcstr
	inc	(sp)
	br	1b
1:
	tst	(sp)+
	jmp	succ
