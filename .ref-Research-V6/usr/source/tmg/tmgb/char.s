f = r5
i = r3
.globl j
.globl jget,iget
.globl char
.globl succ,fail

char:
	jsr	pc,jget
	bne	1f
	tst	(i)+
	jmp fail
1:
	mov	r0,-(sp)
	jsr	pc,iget
	mov	(sp)+,(r0)
	inc	j(f)
	jmp	succ
