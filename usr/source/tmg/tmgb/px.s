f = r5
i = r3
.globl j,n
.globl iget,jget
.globl succ,fail
.globl .px,.pxs

.pxs:
	mov	i,r0
	tst	(i)+
	br	1f
.px:
	jsr	pc,iget
1:
	inc	litc
	mov	n(f),-(sp)
	mov	j(f),-(sp)
	mov	r0,-(sp)
2:
	tstb	*(sp)
	beq	2f
	jsr	pc,jget
	cmpb	r0,*(sp)
	beq	1f
	tst	(sp)+
	mov	(sp)+,j(f)
	mov	(sp)+,n(f)
	jmp	fail
1:
	clr	n(f)
	inc	(sp)
	inc	j(f)
	br 	2b
2:
	cmp	(sp)+,(sp)+
	mov	(sp)+,n(f)
	jmp	succ
litc:	0
