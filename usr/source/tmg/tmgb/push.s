.globl succ
.globl iget
.globl push

push:
	jsr	pc,iget
	mov	(r0),r2
	mov	r2,r1
1:
	mov	r1,-(sp)
	jsr	pc,iget
	mov	(sp)+,r1
	mov	r0,-(sp)
	mov	(r0),-(sp)
	dec	r2
	bgt	1b
	mov	r1,-(sp)
	jsr	pc,succ
/			preserve c bit from here on
	mov	(sp)+,r2
1:
	mov	(sp)+,*(sp)+
	dec	r2
	bgt	1b
	rts	pc	/pass sret or fret back to invoking rule
