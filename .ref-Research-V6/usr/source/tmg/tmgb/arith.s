.globl succ,fail,iget
.globl seekchar,getword,alterword
.globl sprv,update
.globl .l,.u,.p,.t,.st,

/ infix =
.st:
	jsr	pc,sprv
	mov	(sp)+,(sp)
	mov	(sp)+,(sp)
/ update
.u:
	jsr	pc,update
	br	9f
/ pop stack
.p:
	jsr	pc,sprv
	cmp	(sp)+,(sp)+
	br	9f
/ test stack
.t:
	jsr	pc,sprv
	mov	(sp)+,(sp)+
	bne	9f
	jmp	fail
/ load named value
/ rvalue into (sp), lvalue into 2(sp)
.l:
	jsr	pc,iget
	mov	r0,-(sp)
	mov	(r0),-(sp)
	br	9f
/ update a stored value, used by all assignments
update:
	cmp	2+2(sp),$-1
	beq	1f
	mov	0+2(sp),*2+2(sp)
	rts	pc
1:
	mov	4+2(sp),r1
	mov	6+2(sp),r0
	jsr	pc,seekchar
	mov	0+2(sp),r0
	jsr	pc,alterword
/ make sp hold a simple rv (forget it might be a table value)
sprv:
	mov	(sp)+,r0
	cmp	$-1,2(sp)
	bne	1f
	mov	(sp)+,(sp)
	mov	(sp)+,(sp)
1:
	mov	r0,pc
9:
	jmp	succ

