/
/

/ r5 -- long integer arithmetic

.globl	iad4
.globl	isb4
.globl	imp4
.globl	idv4
.globl	ing4
.globl	rerr

iad4:
	jsr	pc,load
	addf	fr0,fr1
	br	store

isb4:
	jsr	pc,load
	subf	fr0,fr1
	br	store

imp4:
	jsr	pc,load
	mulf	fr0,fr1
	br	store

idv4:
	jsr	pc,load
	divf	fr0,fr1

store:
	movfi	fr1,-(sp)
	jmp	*(r4)+

load:
	mov	(sp)+,r0
	setd
	setl
	movif	(sp)+,fr0
	movif	(sp)+,fr1
	jmp	(r0)

ing4:
	neg	(sp)
	neg	2(sp)
	sbc	(sp)
	jmp	*(r4)+

